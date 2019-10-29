# parv/errors.py
#
# Parser / creator errors

# PEP 585 (Consistent with original program style)
from __future__ import annotations


class ParvError(Exception):
    """
    Base class for all errors in Parv.
    """
    ...

class ParseFailure(ParvError):
    """
    Base class for all failures while parsing.
    """
    ...

class CreatorFailure(ParseFailure):
    """
    Creator (not match) failed.
    """

    def __init__(self, creators: tuple[Creator], error: int, index: int):
        """
        Create a CreatorFailure for `creators[error]`. If `error` is -1,
        the failure is attributed to all `creators`. `index` is the
        location of the last successful token.
        """
        super().__init__(creators, error, index)
        self.creators = creators
        self.error = error
        self.index = index

class MatchFailure(ParseFailure):
    """
    Match creator failed.
    """

    def __init__(self, obj: Any, error: Any, index: int):
        """
        Create a MatchFailure for `obj` when `error` was unexpectedly
        received.`index` is the location of the last successful token.
        """
        super().__init__(obj, error, index)
        self.obj = obj
        self.error = error
        self.index = index

class ParseError(ParvError):
    """
    Error occured while parsing (from the ^throw operator).
    """

    def __init__(self, message: str, index: int):
        """
        Create a ParseError with `message` at `index`. This will never
        be caught, unlike ParseFailure and its subclasses.
        """
        super().__init__(message)
        self.message = message
        self.index = index
