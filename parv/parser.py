# parv/parser.py
#
# Parser requests and class.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from .creator import *
from .req import pop, tramp, find, current, reset
from .token import Token


# --- Parser class for mBNF / PEG ---

class Parser:
    r"""
    Simple parser that provides the basic functions for a mBNF / PEG
    parser. This only deals with the data buffering, lookahead and
    backtracking, coroutine trampolining, and rule lookups. Most of the
    work is provided elsewhere such as in Creator or in Visitor.

    Note that `Parser.rules` comes preset with the following:

        "thing": Parser.thing,
        "eof": match(EOFError),
        "tab": literal("\t"),
        "space": literal(" "),
        "newline": literal("\n"),
        "digit": choice_from("0123456789"),
        "letter": choice(ref("lletter"), ref("uletter")),
        "lletter": choice_from("abcdefghijklmnopqrstuvwxyz"),
        "uletter": choice_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        "symbol": choice_from("!@#$%^&*(),./?<>{}[]:;|\\-=_+`~"),

    Here's an calculator using the class oriented method:

        from parv import *
        class Calculator(Parser):
            rules = {
                **Parser.rules,
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
            }

    Here's an example usage of the calculator:

        >>> calc = Calculator(main="expr")
        >>> calc.tokens_from("5+3")
        [<parv.token.Token name='expr' items=['sum']>, <parv.token.Token
        name="<class 'EOFError'>" items=["<class 'EOFError'>"]>]
    """

    # Thing implementation
    async def _thing_coro():
        item = await pop()
        return [Token("thing", item)]
    thing = Creator("thing", "match", _thing_coro)

    # Preset rules for convenience
    #
    # In mBNF:
    # thing ::= #(Parser.thing)
    # eof ::= #(EOFError)
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
        "thing": thing,
        "eof": match(EOFError),
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
        if rules is None:
            rules = {}

        # Table of rule names and their coroutine functions
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
        if self._buffer is not None:
            self.run(eof=True)

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

    def run(self, item: Any = None, *, eof: bool = False) -> list[Token]:
        """
        Return the tokens parsed from `item`. If `eof`, send an EOFError
        and close the parser.
        """
        if self._buffer is None:
            raise RuntimeError("Parser was closed. Create a new one.")
        if self.main is None:
            raise RuntimeError('No specified main rule.')
        if self._parse is None:
            self._parse = self._make_parser_runtime()

        # Result list[Token]
        tokens = []

        if item is not None:
            self._buffer.append(item)
            tokens.extend(self._parse())

        if eof:
            self._buffer.append(EOFError)
            tokens.extend(self._parse())

            # Close parser
            self.index = 0
            self._buffer = None
            self._parse = None

        return tokens

    def tokens_from(self, data: Iterable, *, eof: bool = True) -> list[Token]:
        """
        Return the tokens parsed from each value in `data`.
        """
        tokens = []
        for d in data:
            tokens.extend(self.run(d))
        if eof:
            tokens.extend(self.run(eof=True))
        return tokens

    def _make_parser_runtime(self):
        # Factory function for the main rule (first rule) and EOFError
        main_creator = choice(ref(self.main), match(EOFError))

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
        stack.append(main_creator())

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
                        stack.append(main_creator())
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
                        # End future use of the parser
                        self._buffer = None

                        # Re-raise the error
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
