# trash

i might work on this later, we'll see

```
Expression
(Reference "_expr")

_expr
(Reference "_exprFunction")

Identifier
LiteralValue

Integer
LiteralValue

Float
LiteralValue

Path
LiteralValue

Hpath
LiteralValue

Spath
LiteralValue

Uri
LiteralValue

_exprFunction
(Choice [(Reference "Function"),(Reference "Assert"),(Reference "With"),(Reference "Let"),(Reference "_exprIf")])

Function
(Choice [(Sequence [(Reference "Identifier"),SyntaxValue,(Reference "_exprFunction")]),(Sequence [(Reference "Formals"),SyntaxValue,(Reference "_exprFunction")]),(Sequence [(Reference "Formals"),SyntaxValue,(Reference "Identifier"),SyntaxValue,(Reference "_exprFunction")]),(Sequence [(Reference "Identifier"),SyntaxValue,(Reference "Formals"),SyntaxValue,(Reference "_exprFunction")])])

Formals
(Choice [(Sequence [SyntaxValue,SyntaxValue]),(Sequence [SyntaxValue,(Sequence [(Reference "Formal"),(Repeat (Sequence [SyntaxValue,(Reference "Formal")]))]),SyntaxValue]),(Sequence [SyntaxValue,(Sequence [(Reference "Formal"),(Repeat (Sequence [SyntaxValue,(Reference "Formal")]))]),SyntaxValue,(Reference "Ellipses"),SyntaxValue]),(Sequence [SyntaxValue,(Reference "Ellipses"),SyntaxValue])])

Formal
(Sequence [(Reference "Identifier"),(Choice [(Sequence [SyntaxValue,(Reference "_expr")]),LiteralValue])])

Ellipses
SyntaxValue

Assert
(Sequence [SyntaxValue,(Reference "_expr"),SyntaxValue,(Reference "_exprFunction")])

With
(Sequence [SyntaxValue,(Reference "_expr"),SyntaxValue,(Reference "_exprFunction")])

Let
(Sequence [SyntaxValue,(Choice [(Reference "_binds"),LiteralValue]),SyntaxValue,(Reference "_exprFunction")])

_exprIf
(Choice [(Reference "If"),(Reference "_exprOp")])

If
(Sequence [SyntaxValue,(Reference "_expr"),SyntaxValue,(Reference "_expr"),SyntaxValue,(Reference "_expr")])

_exprOp
(Choice [(Reference "Unary"),(Reference "Binary"),(Reference "_exprApp")])

Unary
(Choice [(Sequence [SyntaxValue,(Reference "_exprOp")]),(Sequence [SyntaxValue,(Reference "_exprOp")])])

Binary
(Choice [(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),SyntaxValue,(Reference "_exprOp")])])

_exprApp
(Choice [(Reference "App"),(Reference "_exprSelect")])

App
(Sequence [(Reference "_exprApp"),(Reference "_exprSelect")])

_exprSelect
(Choice [(Reference "Select"),(Reference "_exprSimple")])

Select
(Choice [(Sequence [(Reference "_exprSimple"),SyntaxValue,(Reference "Attrpath")]),(Sequence [(Reference "_exprSimple"),SyntaxValue,(Reference "Attrpath"),SyntaxValue,(Reference "_exprSelect")])])

_exprSimple
(Choice [(Reference "Identifier"),(Reference "Integer"),(Reference "Float"),(Reference "String"),(Reference "IndentedString"),(Reference "Path"),(Reference "Hpath"),(Reference "Spath"),(Reference "Uri"),(Reference "Parenthesized"),(Reference "Attrset"),(Reference "LetAttrset"),(Reference "RecAttrset"),(Reference "List")])

Parenthesized
(Sequence [SyntaxValue,(Reference "_expr"),SyntaxValue])

Attrset
(Sequence [SyntaxValue,(Choice [(Reference "_binds"),LiteralValue]),SyntaxValue])

LetAttrset
(Sequence [SyntaxValue,SyntaxValue,(Choice [(Reference "_binds"),LiteralValue]),SyntaxValue])

RecAttrset
(Sequence [SyntaxValue,SyntaxValue,(Choice [(Reference "_binds"),LiteralValue]),SyntaxValue])

String
(Sequence [SyntaxValue,(Choice [(Reference "_stringParts"),LiteralValue]),SyntaxValue])

IndentedString
(Sequence [SyntaxValue,(Choice [(Reference "_indStringParts"),LiteralValue]),SyntaxValue])

_stringParts
(Repeat1 (Choice [(Reference "_strContent"),(Reference "Interpolation"),(Reference "EscapeSequence")]))

_indStringParts
(Repeat1 (Choice [(Reference "_indStrContent"),(Reference "Interpolation"),LiteralValue]))

_binds
(Repeat1 (Choice [(Reference "Bind"),(Reference "Inherit")]))

Bind
(Sequence [(Reference "Attrpath"),SyntaxValue,(Reference "_expr"),SyntaxValue])

Inherit
(Choice [(Sequence [SyntaxValue,(Reference "Attrs"),SyntaxValue]),(Sequence [SyntaxValue,(Reference "Parenthesized"),(Reference "Attrs"),SyntaxValue])])

Attrpath
(Sequence [(Reference "_attr"),(Repeat (Sequence [SyntaxValue,(Reference "_attr")]))])

Attrs
(Repeat1 (Reference "_attr"))

_attr
(Choice [(Reference "Identifier"),(Reference "String"),(Reference "Interpolation")])

Interpolation
(Sequence [SyntaxValue,(Reference "_expr"),SyntaxValue])

List
(Sequence [SyntaxValue,(Repeat (Reference "_exprSelect")),SyntaxValue])

Comment
LiteralValue
```
