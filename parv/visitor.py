# parv/visitor.py
#
# Visitor requests and class.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from .req import tramp, evaluate


# --- Token visitor class to implement infinite recursion ---

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
        mode: Literal["top-down", "bottom-up"] = "top-down",
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
        return f"<{ty} mode={self.mode!r}>"

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
