# parv/token.py
#
# Token for use in Parser, Creator, and Visitor

# PEP 585 (Consistent with original program style)
from __future__ import annotations


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
        name = self.name
        items = [t.name for t in self.items] if self.isleaf else [str(self.items[0])]
        frozen = " frozen=True" if self.frozen else ""
        return f"<{ty} {name=} {items=}{frozen}>"

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
