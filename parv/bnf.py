# parv/bnf.py
#
# Simple mBNF / PEG parser and visitor.

# PEP 585 (Consistent with original program style)
from __future__ import annotations

from .creator import *
from .parser import Parser
from .visitor import Visitor

# --- mBNF parser (example subclass from Parser) ---

class BNFParser(Parser):
    """
    Parser for mBNF / PEG grammars. See "docs.python.org" for examples.
    `.run()` and `.tokens_from()` return list[Token] for use with BNFVisitor.
    """

    # New mBNF rules (keep base class's rules too)
    rules = {
        **Parser.rules,
        'line': choice(
            ref('rule'),
            ref('comment'),
            ref('newline'),
            ref('eof'),
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

    # Different default for `main`
    def __init__(self, main="line", rules=None):
        super().__init__(main, rules)


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

    def parser_from(self, tokens):
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
