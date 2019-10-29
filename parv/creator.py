# parv/creator.py
#
# Token creators.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from .errors import *
from .req import *
from .token import Token

# --- Token creator class ---

class Creator:

    types: frozenset[str] = frozenset({
        "match", "throw",
        "wrap", "ref",
        "andpred", "notpred",
        "repeat", "choice", "join",
    })

    def __init__(self, name: str, type_: str, corofunc: Callable) -> None:
        """
        Create a token creator with `name` and `type_`. The underlying
        coroutine function is `corofunc`. For usage in Parser.
        """
        if type_ not in type(self).types:
            raise ValueError(f"Type {type_!r} not in {type(self).__name__}.types")
        self.name = name
        self.type = type_
        self.corofunc = corofunc

    def __call__(self, *args, **kwargs):
        return self.corofunc(*args, **kwargs)

    def __repr__(self):
        name = type(self).__qualname__
        module = type(self).__module__
        if module != "__main__":
            ty = f"{module}.{name}"
        else:
            ty = name
        return f"<{ty} name={self.name!r} type={self.type!r}>"

    @classmethod
    def match(cls, obj: Any) -> cls:
        """
        Return a Creator for `obj` equality.
        """
        async def match():
            index = await current()
            item = await pop()
            if obj != item:
                await reset(index)
                raise MatchFailure(obj, item, index)
            return [Token(repr(obj), obj)]
        return cls(repr(obj), "match", match)

    @classmethod
    def throw(cls, message: Any) -> cls:
        """
        Return a Creator that raises ParseError with `message`.
        """
        async def throw():
            index = await current()
            raise ParseError(message, index)
        return cls(f"(^{message})", "throw", throw)

    @classmethod
    def wrap(cls, name: str, creator: Creator) -> cls:
        """
        Return a corofunc that wraps the `creator`s result with a new
        Token The returned Token will have the original tokens in its
        `.items`.
        """
        async def wrap():
            index = await current()
            try:
                old_tokens = await tramp(creator())
            except ParseFailure as e:
                await reset(index)
                raise CreatorFailure(creator, 0, index) from e
            else:
                new_token = Token(name, *old_tokens)
                return [new_token]
        return cls(name, "wrap", wrap)

    @classmethod
    def ref(cls, name: str) -> cls:
        """
        Return a corofunc that finds a reference to `name`.
        """
        async def ref():
            index = await current()
            creator = await find(name)
            try:
                old_tokens = await tramp(creator())
            except ParseFailure as e:
                await reset(index)
                raise CreatorFailure(creator, 0, index) from e
            else:
                new_token = Token(name, *old_tokens)
                return [new_token]
        return cls(name, "ref", ref)

    @classmethod
    def andpred(cls, creator: Creator) -> cls:
        """
        Return a corofunc that confirms the success of `creator` without
        advancing the index.
        """
        async def andpred():
            index = await current()
            try:
                await tramp(creator())
            except ParseFailure as e:
                raise CreatorFailure(creator, 0, index) from e
            else:
                return []
            finally:
                await reset(index)
        return cls(f"(&{creator.name})", "andpred", andpred)

    @classmethod
    def notpred(cls, creator: Creator) -> cls:
        """
        Return a corofunc that confirms the failure of `creator` without
        advancing the index.
        """
        async def notpred():
            index = await current()
            try:
                await tramp(creator())
            except ParseFailure:
                return []
            else:
                rule = creator.name
                raise CreatorFailure(creator, 0, index)
            finally:
                await reset(index)
        return cls(f"(!{creator.name})", "notpred", notpred)

    @classmethod
    def repeat(cls, creator: Creator, type_: str = '') -> cls:
        """
        Return a corofunc that enforces the 'repeat'
        The 'type_' must be in {'', '?', '+', '*'}
        """
        if type_ not in {'', '?', '+', '*'}:
            raise ValueError(f"Repetition type not valid: {type_!r}")

        async def repeat():
            new_tokens = []
            index = await current()
            try:
                old_tokens = await tramp(creator())
            except ParseFailure as e:
                if type_ in {'', '+'}:
                    raise CreatorFailure(creator, 0, index) from e
                else:
                    await reset(index)
                    return new_tokens
            else:
                new_tokens.extend(old_tokens)

            if type_ in {'+', '*'}:
                while True:
                    index = await current()
                    try:
                        old_tokens = await tramp(creator())
                    except ParseFailure:
                        await reset(index)
                        break
                    else:
                        new_tokens.extend(old_tokens)
            return new_tokens

        return cls(f"({creator.name}{type_})", "repeat", repeat)

    @classmethod
    def choice(cls, *creators: Creator) -> cls:
        """
        Return a corofunc that tries all `creators` until success
        If all `creators` fail, the cumulative errors will be stored
        in the error's `.children`.
        """
        async def choice():
            errors = []

            index = await current()
            for creator in creators:
                try:
                    tokens = await tramp(creator())
                except ParseFailure as e:
                    await reset(index)
                    errors.append(e)
                else:
                    return tokens

            err = CreatorFailure(creators, -1, index)
            err.children = errors
            raise err

        rule = " | ".join(c.name for c in creators)
        return cls(f"({rule})", "choice", choice)

    @classmethod
    def join(cls, *creators: Creator) -> cls:
        """
        Return a corofunc that runs through all `creators`.
        """
        async def join():
            new_tokens = []

            try:
                for i, creator in enumerate(creators):
                    index = await current()
                    old_tokens = await tramp(creator())
                    new_tokens.extend(old_tokens)
            except ParseFailure as e:
                await reset(index)
                raise CreatorFailure(creators, i, index) from e
            else:
                return new_tokens

        rule = " ".join(c.name for c in creators)
        return cls(f"({rule})", "join", join)


# --- Convenience global creator functions ---

match = Creator.match
throw = Creator.throw
wrap = Creator.wrap
ref = Creator.ref
andpred = Creator.andpred
notpred = Creator.notpred
repeat = Creator.repeat
choice = Creator.choice
join = Creator.join

def literal(str_: str):
    return wrap(repr(str_), join(*(match(s) for s in str_)))

def one_zero(creator: Creator):
    return repeat(creator, "?")

def one_more(creator: Creator):
    return repeat(creator, "+")

def zero_more(creator: Creator):
    return repeat(creator, "*")

def choice_from(str_: str):
    return choice(*(match(s) for s in str_))
