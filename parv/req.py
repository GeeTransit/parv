# parv/req.py
#
# Parser and visitor requests.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from types import coroutine

# Only way to "yield" to the parser / visitor.
@coroutine
def _yield(*request):
    result = yield request
    if isinstance(result, BaseException):
        raise result
    else:
        return result


# --- Parser / visitor requests ---

async def pop():
    """
    Return the next item, advancing the current index by 1.
    """
    while True:
        data = await _yield("req_pop")
        if data is not None:
            return data

async def tramp(coro: Coroutine):
    """
    Trampoline `coro` to a lower stack level and return its result.
    """
    return await _yield("req_tramp", coro)

async def find(name: str):
    """
    Find `name` in the parser's rules and return it.
    """
    return await _yield("req_find", name)

async def current():
    """
    Return the current index.
    """
    return await _yield("req_current")

async def reset(last: int):
    """
    Reset the current index to `last`. Raises ValueError if `last` is
    larger that the current index.
    """
    await _yield("req_reset", last)

async def evaluate(name: str, token: Token):
    """
    Find `name` in the visitor's rules and return its result.
    """
    return await _yield("req_evaluate", name, token)
