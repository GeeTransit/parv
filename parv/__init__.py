# parv/__init__.py
#
# Simple mBNF / PEG parser and visitor.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from functools import wraps
from types import coroutine


# Only way to "yield" to the parser loop.
@coroutine
def _yield(*request):
    result = yield request
    if isinstance(result, BaseException):
        raise result
    else:
        return result


# --- Low level parser requests ---

async def _pop():
    return await _yield("req_pop")

async def _tramp(coro: Coroutine):
    return await _yield("req_tramp", coro)

async def _find(name: str):
    return await _yield("req_find", name)

async def _current():
    return await _yield("req_current")

async def _reset(last: int):
    return await _yield("req_reset", last)


# --- High level parser requests ---

async def pop():
    """
    Return the next item, advancing the current index by 1.
    """
    while True:
        data = await _pop()
        if data is not None:
            return data

async def tramp(coro: Coroutine):
    """
    Trampoline `coro` to a lower stack level and return its result.
    """
    return await _tramp(coro)

async def find(name: str):
    """
    Find `name` in the parser's rules and return it.
    """
    return await _find(name)

async def current():
    """
    Return the current index.
    """
    return await _current()

async def reset(last: int):
    """
    Reset the current index to `last`. Raises ValueError if `last` is
    larger that the current index.
    """
    await _reset(last)


# --- Failure exception types ---

class ParseFailure(Exception):
    pass

class CreatorFailure(ParseFailure):
    def __init__(self, index: int, *creators: Creator):
        """
        Create a CreatorFailure for `creators[index]`. If `index` is -1,
        the failure is attributed to all `creators`.
        """
        super().__init__(index, *creators)
        self.index = index
        self.creators = creators

class MatchFailure(ParseFailure):
    def __init__(self, obj: Any, error: Any):
        """
        Create a MatchFailure for `obj` when `error` was unexpectedly
        received.
        """
        super().__init__(obj, error)
        self.obj = obj
        self.error = error


# --- Error exception types ---

class ParseError(Exception):
    def __init__(self, message: str, index: int):
        """
        Create a ParseError with `message` at `index`. This will never
        be caught, unlike ParseFailure and its subclasses.
        """
        super().__init__(message)
        self.message = message
        self.index = index


# --- Token Creator class ---

class Creator:

    types = frozenset({
        "match", "throw",
        "wrap", "ref",
        "andpred", "notpred",
        "repeat", "choice", "join",
    })

    def __init__(self, name: str, type_: str, corofunc: Callable):
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
    def match(cls, obj: Any):
        """
        Return a Creator for `obj` equality.
        """
        async def match():
            index = await current()
            item = await pop()
            if obj != item:
                await reset(index)
                raise MatchFailure(obj, (item, index))
            return [Token(repr(obj), obj)]
        return cls(repr(obj), "match", match)

    @classmethod
    def throw(cls, message: Any):
        """
        Return a Creator that raises ParseError with `message`.
        """
        async def throw():
            index = await current()
            raise ParseError(message, index)
        return cls(f"(^{message})", "throw", throw)

    @classmethod
    def wrap(cls, name: str, creator: Creator):
        """
        Return a corofunc that wraps the `creator`s result with a new
        Token The returned Token will have the original tokens in its
        `.items`.
        """
        async def wrap():
            try:
                old_tokens = await tramp(creator())
            except ParseFailure as e:
                raise CreatorFailure(0, creator) from e
            else:
                new_token = Token(name, *old_tokens)
                return [new_token]
        return cls(name, "wrap", wrap)

    @classmethod
    def ref(cls, name: str):
        """
        Return a corofunc that finds a reference to `name`.
        """
        async def ref():
            creator = await find(name)
            try:
                old_tokens = await tramp(creator())
            except ParseFailure as e:
                raise CreatorFailure(0, creator) from e
            else:
                new_token = Token(name, *old_tokens)
                return [new_token]
        return cls(name, "ref", ref)

    @classmethod
    def andpred(cls, creator: Creator):
        """
        Return a corofunc that confirms the success of `creator` without
        advancing the index.
        """
        async def andpred():
            index = await current()
            try:
                await tramp(creator())
            except ParseFailure as e:
                raise CreatorFailure(0, creator) from e
            else:
                return []
            finally:
                await reset(index)
        return cls(f"(&{creator.name})", "andpred", andpred)

    @classmethod
    def notpred(cls, creator: Creator):
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
                raise CreatorFailure(0, creator)
            finally:
                await reset(index)
        return cls(f"(!{creator.name})", "notpred", notpred)

    @classmethod
    def repeat(cls, creator: Creator, type_: str = ''):
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
                    raise CreatorFailure(0, creator) from e
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
    def choice(cls, *creators: Creator):
        """
        Return a corofunc that tries all `creators` until success
        If all `creators` fail, the cumulative errors will be stored
        in the error's `.children`.
        """
        async def choice():
            errors = []

            for creator in creators:
                index = await current()
                try:
                    tokens = await tramp(creator())
                except ParseFailure as e:
                    await reset(index)
                    errors.append(e)
                else:
                    return tokens

            err = CreatorFailure(-1, *creators)
            err.children = errors
            raise err

        rule = " | ".join(c.name for c in creators)
        return cls(f"({rule})", "choice", choice)

    @classmethod
    def join(cls, *creators: Creator):
        """
        Return a corofunc that runs through all `creators`.
        """
        async def join():
            new_tokens = []

            try:
                for i, creator in enumerate(creators):
                    old_tokens = await tramp(creator())
                    new_tokens.extend(old_tokens)
            except ParseFailure as e:
                if len(creators) == 1:
                    raise
                else:
                    raise CreatorFailure(i, *creators) from e
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


# --- Parser class for mBNF / PEG ---

class Parser:
    r"""
    Simple parser that provides the basic functions for a mBNF / PEG
    parser. This only deals with the data buffering, lookahead and
    backtracking, coroutine trampolining, and rule lookups. Most of the
    work is provided elsewhere such as in Creator or in Visitor.

    Note that `Parser.rules` comes preset with the following:

        "tab": literal("\t"),
        "space": literal(" "),
        "newline": literal("\n"),
        "digit": choice_from("0123456789"),
        "letter": choice(ref("lletter"), ref("uletter")),
        "lletter": choice_from("abcdefghijklmnopqrstuvwxyz"),
        "uletter": choice_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        "symbol": choice_from("!@#$%^&*(),./?<>{}[]:;|\\-=_+`~"),

    Here's an example for a simple calculator:

        calc = Parser.from_peg('''
        expr ::= sum
        sum ::= product (("+", "-") product)*
        product ::= value (("*", "/") value)*
        value ::= digit+ / "(" expr ")"
        ''')

    Here's an alternative using the class oriented method:

        class calc(Parser):
            @Parser.define
            async def expr(self):
                return await ref("sum")
            @Parser.define
            async def sum(self):
                return await join(
                    ref("product"),
                    zero_more(join(
                        choice(literal("+"), literal("-")),
                        ref("product"),
                    )),
                )
            @Parser.define
            async def product(self):
                return await join(
                    ref("value"),
                    zero_more(join(
                        choice_from(literal("*"), literal("/")),
                        ref("value"),
                    )),
                )
            @Parser.define
            async def value(self):
                return await choice(
                    one_more(ref("digit")),
                    join(
                        literal("("),
                        ref("expr"),
                        literal(")"),
                    ),
                )

    Here's the same below the surface:

        calc = Parser("expr", {
            "expr": ref("sum"),
            "sum": join(
                ref("product"),
                zero_more(join(
                    choice_from("+-"),
                    ref("product"),
                )),
            ),
            "product": join(
                ref("value"),
                zero_more(join(
                    choice_from("*/"),
                    ref("value"),
                )),
            ),
            "value": choice(
                one_more(ref("digit")),
                join(
                    literal("("),
                    ref("expr"),
                    literal(")"),
                ),
            ),
        })
    """

    # Preset rules for convenience
    #
    # In mBNF:
    # tab ::= "\t"
    # space ::= " "
    # newline ::= "\n"
    # digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    # letter ::= lletter | uletter
    # lletter ::= (
    #     "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
    #     | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t"
    #     | "u" | "v" | "w" | "x" | "y" | "z"
    # )
    # uletter ::= (
    #     "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
    #     | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
    #     | "U" | "V" | "W" | "X" | "Y" | "Z"
    # )
    # symbol ::= (
    #     "!" | "@" | "#" | "$" | "%" | "^" | "&" | "*" | "(" | ")"
    #     | "," | "." | "/" | "?" | "<" | ">" | "{" | "}" | "[" | "]"
    #     | ":" | ";" | "|" | "\\" | "-" | "=" | "_" | "+" | "`" | "~"
    # )
    rules: dict[str, Callable] = {
        "tab": literal("\t"),
        "space": literal(" "),
        "newline": literal("\n"),
        "digit": choice_from("0123456789"),
        "letter": choice(ref("lletter"), ref("uletter")),
        "lletter": choice_from("abcdefghijklmnopqrstuvwxyz"),
        "uletter": choice_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        "symbol": choice_from("!@#$%^&*(),./?<>{}[]:;|\\-=_+`~"),
    }

    def __init__(
        self,
        main: Optional[str] = None,
        rules: Optional[dict[str, Creator]] = None,
    ):
        """
        Create a new parser.
        `main` is the name of the main rule.
        `rules` is a dict[str, Creator] of all rules.
        """
        # Table of rule names and their coroutine functions
        if rules is None:
            rules = {}
        self.rules = {**type(self).rules, **rules}
        # PEP 584 alternative  (+ and += operators for dict)
        # self.rules = type(self).rules + rules

        # Main rule name
        self.main = main

        # Index of parser (in self._buffer)
        self.index = 0

        # Buffer stack of data
        self._buffer = []

        # Parsing function (created in _make_parser_runtime)
        self._parse = None

    def __repr__(self):
        name = type(self).__qualname__
        module = type(self).__module__
        if module != "__main__":
            ty = f"{module}.{name}"
        else:
            ty = name
        return f"<{ty} main={self.main!r}>"

    def __enter__(self):
        return self

    def __exit__(self, ty, val, tb):
        self.close()

    @classmethod
    def define(
        cls,
        func: Optional[Callable] = None,
        *,
        name: Optional[str] = None,
        rules: Optional[dict[str, Creator]] = None,
    ):
        """
        Update `rules` with `func`, using `name` if specified.
        """
        # Create the decorator function
        def _update_rules(func: Callable):
            # Get a rule name
            if name is None:
                func_name = func.__name__
            else:
                func_name = name

            # Get the target rule dict
            if rules is None:
                func_rules = cls.rules
            else:
                func_rules = rules

            # Update rules with `func`
            func_rules[func_name] = func
            return func

        # Check if it was called as a decorator or as a function
        if func is None:
            return _update_rules
        else:
            return _update_rules(func)

    def copy(self) -> Parser:
        """
        Return a fresh copy of self.
        """
        return type(self)(self.main, self.rules.copy())

    def close(self) -> None:
        """
        Close the parser and prevent future usage.
        """
        self.index = 0
        self._buffer = None
        self._parse = None

    def add(self, data: Any) -> None:
        """
        Add `data` into the buffer.
        """
        if self._buffer is None:
            raise RuntimeError("Parser was closed. Create a new one.")
        if self._parse is None:
            self._parse = self._make_parser_runtime()
        self._buffer.extend(data)

    def get(self) -> list[Token]:
        """
        Return the tokens parsed from the buffer.
        """
        if self._buffer is None:
            raise RuntimeError("Parser was closed. Create a new one.")
        if self.main is None:
            raise RuntimeError('No specified main rule.')
        if self._parse is None:
            self._parse = self._make_parser_runtime()
        return self._parse()

    def tokens_from(self, data: Iterable) -> list[Token]:
        """
        Return the tokens parsed from each value in `data`.
        """
        tokens = []
        for d in data:
            self.add(d)
            tokens.extend(self.get())
        return tokens

    def _make_parser_runtime(self):
        # Factory function for the main rule (first rule)
        mainref = ref(self.main)

        # Stack of running coroutines
        stack = []

        # Save exception info to prevent their tracebacks from becoming
        # too long
        last_exc = last_tb = None

        # Flag to indicate request for more data
        running = True

        def req_pop():
            nonlocal running

            # Check if coroutine needs more data
            if len(self._buffer) <= self.index:
                running = False
                return

            # Return the next index from the buffer
            value = self._buffer[self.index]
            self.index += 1
            return value

        def req_tramp(coro: Coroutine):
            stack.append(coro)

        def req_find(name: str):
            try:
                return self.rules[name]
            except KeyError as e:
                return e

        def req_current():
            return self.index

        def req_reset(last: int):
            if last > self.index:
                return ValueError(f"Index larger than current: {last} > {self.index}")
            self.index = last

        # Table of request names and the generator function
        requests = {
            name: func
            for name, func in locals().items()
            if name.startswith("req_")
        }

        # Add the initial coroutine
        stack.append(mainref())

        # Main parser loop. This controls all coroutine trampolines and
        # provides backtracking functions.
        def parse_run():
            # Exception handling
            nonlocal last_exc, last_tb

            # Flag for more data
            nonlocal running
            running = True

            # Completed tokens
            tokens = []

            # Next value to send to top of stack
            value = None

            # ~ Loop until you can't no more ~
            while running:

                # Run through the coroutine
                try:
                    req = stack[-1].send(value)

                # Successfully created a token :D
                except StopIteration as e:
                    # Clear last exception
                    last_exc = last_tb = None

                    # Remove completed coroutine
                    stack.pop()

                    # Check if main rule succeeded
                    if not stack:
                        # Add to token list
                        tokens.extend(e.value)

                        # Restart the stack with the main rule
                        stack.append(mainref())
                        value = None

                    else:
                        # Relay the result
                        value = e.value

                # An error happened :/
                except BaseException as e:
                    # Remove terminated coroutine
                    stack.pop()

                    # Check if main rule failed / errored
                    if not stack:
                        raise

                    # Check if exception was the same as last time ._.
                    elif last_exc is e:
                        # Revert changes to the traceback
                        last_exc.__traceback__ = last_tb
                        # Keep the last exception object
                        value = last_exc

                    else:
                        # Update the last exception
                        value = last_exc = e
                        last_tb = e.__traceback__

                # All is good!
                else:
                    # Try requesting the requester's request with the requests requested
                    # (Try running the wanted function with the arguments provided)
                    try:
                        value = requests[req[0]](*req[1:])

                    # Error in parsing code
                    except BaseException as e:
                        # End future use of the parser
                        self._buffer = None

                        # We'll bail out
                        raise

            # Return the parsed tokens
            return tokens

        # Return the parsing function
        return parse_run


# --- mBNF parser (example subclass from Parser) ---

class BNFParser(Parser):
    """
    Parser for mBNF / PEG grammars. See "docs.python.org" for examples.
    `.get()` and `.tokens_from()` return list[Token] for use with BNFVisitor.
    """

    # New mBNF rules (keep base class's rules too)
    rules = {
        **Parser.rules,
        'line': choice(
            ref('rule'),
            ref('comment'),
            ref('newline')
        ),
        'rule': join(
            ref('name'),
            ref('optws'),
            literal("::="),
            ref('optws'),
            ref('expression'),
            ref('newline'),
        ),
        "comment": join(
            literal("#"),
            zero_more(choice(
                ref("all"),
                literal("'"),
                literal('"'),
            )),
            ref("newline"),
        ),
        "expression": join(
            ref("list"),
            zero_more(join(
                ref("optws"),
                choice_from("|/"),
                ref("optws"),
                ref("list"),
            )),
        ),
        "list": join(
            ref("term"),
            zero_more(join(
                ref("ws"),
                ref("term"),
            )),
        ),
        "term": choice(
            ref("pred"),
            ref("repeat"),
            ref("throw"),
            ref("opt"),
        ),
        "pred": join(
            choice_from("&!"),
            ref("unit"),
        ),
        "repeat": join(
            ref("unit"),
            one_zero(choice_from("?+*")),
        ),
        "unit": choice(
            ref("literal"),
            ref("group"),
            ref("name"),
        ),
        "group": join(
            literal("("),
            ref("optws"),
            ref("expression"),
            ref("optws"),
            literal(")"),
        ),
        "opt": join(
            literal("["),
            ref("optws"),
            ref("expression"),
            ref("optws"),
            literal("]"),
        ),
        "throw": join(
            literal("^"),
            ref("literal"),
        ),
        "literal": choice(
            ref("sliteral"),
            ref("dliteral"),
        ),
        "sliteral": join(
            literal("'"),
            zero_more(choice(
                ref("all"),
                literal('"'),
            )),
            literal("'"),
        ),
        "dliteral": join(
            literal('"'),
            zero_more(choice(
                ref("all"),
                literal("'"),
            )),
            literal('"'),
        ),
        "name": join(
            ref("letter"),
            zero_more(choice(
                ref("letter"),
                literal("_"),
            )),
        ),
        "all": choice(
            ref("tab"),
            ref("space"),
            ref("digit"),
            ref("letter"),
            ref("symbol"),
        ),
        "ws": one_more(choice(
            ref("tab"),
            ref("space"),
        )),
        "optws": one_zero(ref("ws")),
    }

    # Different default `main`: "line"
    def __init__(
        self,
        main: Optional[str] = "line",
        rules: Optional[dict[str, Creator]] = None,
    ):
        super().__init__(main, rules)


# --- Token class to store CST ---

class Token:
    """
    Simple token that holds a name and its children tokens.
    """

    def __init__(self, name: str, *items, frozen: bool = False):
        """
        Create a token with name as `name` and its items as `items".
        If `frozen`, the token acts as a leaf node.
        """
        # if not items:
        #     raise ValueError("`items` cannot be empty")
        if frozen and len(items) != 1:
            raise ValueError("`items` must have length 1 when `frozen` is True")
        if len(items) > 1 and not all(isinstance(i, Token) for i in items):
            raise ValueError("`items` must all be Token if `frozen` is False")
        self.name = name
        self.items = items
        self.frozen = frozen

    def __repr__(self):
        name = type(self).__qualname__
        module = type(self).__module__
        if module != "__main__":
            ty = f"{module}.{name}"
        else:
            ty = name
        if self.items:
            sep = ", "
            items = ", ".join(repr(i) for i in self.items)
        else:
            sep = ""
            items = ""
        if self.frozen:
            return f"{ty}({self.name!r}{sep}{items}, frozen=True)"
        else:
            return f"{ty}({self.name!r}{sep}{items})"

    def __str__(self):
        return "".join(str(t) for t in self.items)

    def __hash__(self):
        return hash((self.name, self.items, self.frozen))

    def __eq__(self, other: Any):
        if not isinstance(other, type(self)):
            return NotImplemented
        return self.items == other.items

    def __ne__(self, other: Any):
        if not isinstance(other, type(self)):
            return NotImplemented
        return self.items != other.items

    def __iter__(self):
        if self.frozen:
            yield str(self.items[0])
        else:
            yield from self.items

    def __len__(self):
        if self.frozen:
            return 1
        else:
            return len(self.items)

    def __getitem__(self, index):
        if self.frozen:
            if index != 0:
                raise IndexError
            return str(self.items[0])
        else:
            return self.items[index]

    @property
    def isleaf(self):
        if self.frozen:
            return True
        elif len(self.items) == 1 and isinstance(self.items[0], str):
            return True
        else:
            return False


# --- Low level visitor requests

async def _tramp(coro: Coroutine):
    return await _yield("req_tramp", coro)

async def _evaluate(name: str, token: Token):
    return await _yield("req_evaluate", name, token)


# --- High level visitor requests ---

async def tramp(coro: Coroutine):
    """
    Trampoline `coro` to a lower stack level and return its result.
    """
    return await _tramp(coro)

async def evaluate(name: str, token: Token):
    """
    Find `name` in the visitor's rules and return its result.
    """
    return await _evaluate(name, token)


# --- Token Visitor class to implement infinite recursion ---

class Visitor:
    """
    Simple token visitor that implements infinite recursion. Supports
    top-down and bottom-up evaluation.
    """

    # Default rules (useful for subclasses)
    rules: dict[str, Callable] = {}

    def __init__(
        self,
        rules: Optional[dict[str, Callable]] = None,
        mode: typing.Literal["top-down", "bottom-up"] = "top-down",
    ):
        """
        Create a visitor.
        `rules` is a dict[str, Callable] of all rules.
        `mode` is a Literal["top-down", "bottom-up"] of the evaluation method.
        """
        if not isinstance(mode, str):
            raise TypeError(f"mode must be of type str, not {type(mode).__qualname__}")
        if mode not in {"top-down", "bottom-up"}:
            raise ValueError(f"mode must be 'top-down' or 'bottom-up'")

        # Table of rule names and their coroutine functions
        if rules is None:
            rules = {}
        self.rules = {**type(self).rules, **rules}
        # PEP 584 alternative  (+ and += operators for dict)
        # self.rules = type(self).rules + rules

        # Evaluation method
        self.mode = mode

        # Visitor function (created in _make_visitor_runtime)
        self._visit = None

    def __repr__(self):
        name = type(self).__qualname__
        module = type(self).__module__
        if module != "__main__":
            ty = f"{module}.{name}"
        else:
            ty = name
        return f"{ty}({self.token!r}, mode={self.mode!r})"

    @classmethod
    def define(
        cls,
        func: Optional[Callable] = None,
        *,
        name: Optional[str] = None,
        rules: Optional[dict[str, Creator]] = None,
    ):
        """
        Update `rules` with `func`, using `name` if specified.
        """
        # Create the decorator function
        def _update_rules(func: Callable):
            # Get a rule name
            if name is None:
                func_name = func.__name__
            else:
                func_name = name

            # Get the target rule dict
            if rules is None:
                func_rules = cls.rules
            else:
                func_rules = rules

            # Update rules with `func`
            func_rules[func_name] = func
            return func

        # Check if it was called as a decorator or as a function
        if func is None:
            return _update_rules
        else:
            return _update_rules(func)

    def run(self, token: Token) -> Any:
        """
        Evaluate and return the result from the token.
        """
        if self._visit is None:
            self._visit = self._make_visitor_runtime()
        result = self._visit(token)
        return result

    def _make_visitor_runtime(self):
        # Stack of running coroutines
        stack = []

        def req_tramp(coro: Coroutine):
            stack.append(coro)

        def req_evaluate(name: str, token: Token):
            if self.mode == "top-down":
                try:
                    evaluator = self.rules.get(name, _default_evaluator)
                except KeyError as e:
                    return e
                else:
                    coro = evaluator(token)
                    stack.append(coro)
            else:
                try:
                    return completed[(name, token)]
                except KeyError as e:
                    raise RuntimeError(f"Unexpected completed key: {name, token}")

        async def _default_evaluator(token):
            if token.isleaf:
                return token
            else:
                return [await evaluate(t.name, t) for t in token]

        async def _main_evaluator(token):
            if self.mode == "top-down":
                return await evaluate(token.name, token)
            else:
                if not token.isleaf:
                    for t in token:
                        await tramp(main_evaluator(t))
                evaluator = self.rules.get(token.name, _default_evaluator)
                result = await tramp(evaluator(token))
                completed[(token.name, token)] = result
                return result

        # Table of request names and the generator function
        requests = {
            name: func
            for name, func in locals().items()
            if name.startswith("req_")
        }

        # Main parser loop. This controls all coroutine trampolines and
        # provides evaluation functions.
        def visit_run(token: Token):
            # Completed values (only used if mode == "bottom-up")
            completed = {}

            # Next value to send to top of stack
            value = None

            # Save exception info to prevent their tracebacks from
            # becoming too long
            last_exc = last_tb = None

            # Add the initial coroutine
            stack.append(_main_evaluator(token))

            # ~ Loop until you can't no more ~
            while True:

                # Run through the coroutine
                try:
                    req = stack[-1].send(value)

                # Successfully created a token :D
                except StopIteration as e:
                    # Clear last exception
                    last_exc = last_tb = None

                    # Remove completed coroutine
                    stack.pop()

                    # Check if main rule succeeded
                    if not stack:
                        # Return the result
                        return e.value

                    else:
                        # Relay the result
                        value = e.value

                # An error happened :/
                except BaseException as e:
                    # Remove terminated coroutine
                    stack.pop()

                    # Check if main rule failed / errored
                    if not stack:
                        raise

                    # Check if exception was the same as last time ._.
                    elif last_exc is e:
                        # Revert changes to the traceback
                        last_exc.__traceback__ = last_tb
                        # Keep the last exception object
                        value = last_exc

                    else:
                        # Update the last exception
                        value = last_exc = e
                        last_tb = e.__traceback__

                # All is good!
                else:
                    # Try running the request
                    value = requests[req[0]](*req[1:])

        # Return the visiting function
        return visit_run


# --- mBNF rule creator (example subclass from Visitor) ---

class BNFVisitor(Visitor):
    """
    Visitor for mBNF / PEG grammars. See "docs.python.org" for examples.
    `.run()` returns None or tuple[str, Creator].
    `.parser_from()` returns a Parser made from tokens from BNFParser.
    """

    # Create a new rules dict
    rules = {**Visitor.rules}
    _define = Visitor.define(rules=rules)

    def parser_from(self, tokens: Iterable):
        """
        Return a Parser created from `tokens`.
        """
        main_name = None
        rules = {}

        # Visit all tokens
        for token in tokens:
            result = self.run(token)
            if result is not None:
                name, creator = result
                if main_name is None:
                    main_name = name
                rules[name] = creator

        # There's no main rule?!
        if main_name is None:
            raise ValueError("No main rule found")

        # Create the parser from the rules
        return Parser(main_name, rules)

    @_define
    async def line(token):
        t = token[0]
        if t.name == "rule":
            return await evaluate(t.name, t)
        else:
            return None

    @_define
    async def rule(token):
        name, _, _, _, expression, _ = token
        creator = await evaluate(expression.name, expression)
        return str(name), creator

    @_define
    async def expression(token):
        creators = []
        titer = iter(token)
        for t in titer:
            creators.append(await evaluate(t.name, t))
            for _ in range(3):
                next(titer, None)
        return choice(*creators)

    @_define
    async def list(token):
        creators = []
        titer = iter(token)
        for t in titer:
            creators.append(await evaluate(t.name, t))
            next(titer, None)
        return join(*creators)

    @_define
    async def term(token):
        t = token[0]
        return await evaluate(t.name, t)

    @_define
    async def pred(token):
        ty, unit = token
        creator = await evaluate(unit.name, unit)
        if str(ty) == "&":
            return andpred(creator)
        else:
            return notpred(creator)

    @_define
    async def repeat(token):
        unit, *ty = token
        creator = await evaluate(unit.name, unit)
        if not ty:
            return creator
        else:
            return repeat(creator, str(ty[0]))

    @_define
    async def throw(token):
        _, literal = token
        return throw(str(literal))

    @_define
    async def opt(token):
        _, _, expression, _, _ = token
        creator = await evaluate(expression.name, expression)
        return one_zero(creator)

    @_define
    async def unit(token):
        t = token[0]
        return await evaluate(t.name, t)

    @_define
    async def literal(token):
        t = token[0]
        return literal(str(t)[1:-1])

    @_define
    async def group(token):
        _, _, expression, _, _ = token
        return await evaluate(expression.name, expression)

    @_define
    async def name(token):
        return ref(str(token))
