# trash

i might work on this later, we'll see

## output

```
Expression
(Reference "_expr")
Expression ExprArgument


_expr
(Reference "_exprFunction")
no constructor

Identifier
LiteralValue
Identifier StringArgument


Integer
LiteralValue
Integer StringArgument


Float
LiteralValue
Float StringArgument


Path
LiteralValue
Path StringArgument


Hpath
LiteralValue
Hpath StringArgument


Spath
LiteralValue
Spath StringArgument


Uri
LiteralValue
Uri StringArgument


_exprFunction
(Choice [(Reference "Function"),(Reference "Assert"),(Reference "With"),(Reference "Let"),(Reference "_exprIf")])
no constructor

Function
(Choice [(Sequence [(Reference "Identifier"),(SyntaxValue ":"),(Reference "_exprFunction")]),(Sequence [(Reference "Formals"),(SyntaxValue ":"),(Reference "_exprFunction")]),(Sequence [(Reference "Formals"),(SyntaxValue "@"),(Reference "Identifier"),(SyntaxValue ":"),(Reference "_exprFunction")]),(Sequence [(Reference "Identifier"),(SyntaxValue "@"),(Reference "Formals"),(SyntaxValue ":"),(Reference "_exprFunction")])])
Function ExprArgument


Formals
(Choice [(Sequence [(SyntaxValue "{"),(SyntaxValue "}")]),(Sequence [(SyntaxValue "{"),(Sequence [(Reference "Formal"),(Repeat (Sequence [(SyntaxValue ","),(Reference "Formal")]))]),(SyntaxValue "}")]),(Sequence [(SyntaxValue "{"),(Sequence [(Reference "Formal"),(Repeat (Sequence [(SyntaxValue ","),(Reference "Formal")]))]),(SyntaxValue ","),(Reference "Ellipses"),(SyntaxValue "}")]),(Sequence [(SyntaxValue "{"),(Reference "Ellipses"),(SyntaxValue "}")])])
Formals ExprArgument


Formal
(Sequence [(Reference "Identifier"),(Choice [(Sequence [(SyntaxValue "?"),(Reference "_expr")]),LiteralValue])])
Formal ExprArgument ExprArgument


Ellipses
(SyntaxValue "...")
Ellipses 


Assert
(Sequence [(SyntaxValue "assert"),(Reference "_expr"),(SyntaxValue ";"),(Reference "_exprFunction")])
Assert ExprArgument ExprArgument


With
(Sequence [(SyntaxValue "with"),(Reference "_expr"),(SyntaxValue ";"),(Reference "_exprFunction")])
With ExprArgument ExprArgument


Let
(Sequence [(SyntaxValue "let"),(Choice [(Reference "_binds"),LiteralValue]),(SyntaxValue "in"),(Reference "_exprFunction")])
Let ExprArgument ExprArgument


_exprIf
(Choice [(Reference "If"),(Reference "_exprOp")])
no constructor

If
(Sequence [(SyntaxValue "if"),(Reference "_expr"),(SyntaxValue "then"),(Reference "_expr"),(SyntaxValue "else"),(Reference "_expr")])
If ExprArgument ExprArgument ExprArgument


_exprOp
(Choice [(Reference "Unary"),(Reference "Binary"),(Reference "_exprApp")])
no constructor

Unary
(Choice [(Sequence [(SyntaxValue "!"),(Reference "_exprOp")]),(Sequence [(SyntaxValue "-"),(Reference "_exprOp")])])
Unary ExprArgument


Binary
(Choice [(Sequence [(Reference "_exprOp"),(SyntaxValue "=="),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "!="),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "<"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "<="),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue ">"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue ">="),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "&&"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "||"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "->"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "//"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "?"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "+"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "-"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "*"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "/"),(Reference "_exprOp")]),(Sequence [(Reference "_exprOp"),(SyntaxValue "++"),(Reference "_exprOp")])])
Binary ExprArgument


_exprApp
(Choice [(Reference "App"),(Reference "_exprSelect")])
no constructor

App
(Sequence [(Reference "_exprApp"),(Reference "_exprSelect")])
App ExprArgument ExprArgument


_exprSelect
(Choice [(Reference "Select"),(Reference "_exprSimple")])
no constructor

Select
(Choice [(Sequence [(Reference "_exprSimple"),(SyntaxValue "."),(Reference "Attrpath")]),(Sequence [(Reference "_exprSimple"),(SyntaxValue "."),(Reference "Attrpath"),(SyntaxValue "or"),(Reference "_exprSelect")])])
Select ExprArgument


_exprSimple
(Choice [(Reference "Identifier"),(Reference "Integer"),(Reference "Float"),(Reference "String"),(Reference "IndentedString"),(Reference "Path"),(Reference "Hpath"),(Reference "Spath"),(Reference "Uri"),(Reference "Parenthesized"),(Reference "Attrset"),(Reference "LetAttrset"),(Reference "RecAttrset"),(Reference "List")])
no constructor

Parenthesized
(Sequence [(SyntaxValue "("),(Reference "_expr"),(SyntaxValue ")")])
Parenthesized ExprArgument


Attrset
(Sequence [(SyntaxValue "{"),(Choice [(Reference "_binds"),LiteralValue]),(SyntaxValue "}")])
Attrset ExprArgument


LetAttrset
(Sequence [(SyntaxValue "let"),(SyntaxValue "{"),(Choice [(Reference "_binds"),LiteralValue]),(SyntaxValue "}")])
LetAttrset ExprArgument


RecAttrset
(Sequence [(SyntaxValue "rec"),(SyntaxValue "{"),(Choice [(Reference "_binds"),LiteralValue]),(SyntaxValue "}")])
RecAttrset ExprArgument


String
(Sequence [(SyntaxValue "\""),(Choice [(Reference "_stringParts"),LiteralValue]),(SyntaxValue "\"")])
String ExprArgument


IndentedString
(Sequence [(SyntaxValue "''"),(Choice [(Reference "_indStringParts"),LiteralValue]),(SyntaxValue "''")])
IndentedString ExprArgument


_stringParts
(Repeat1 (Choice [(Reference "_strContent"),(Reference "Interpolation"),(Reference "EscapeSequence")]))
no constructor

_indStringParts
(Repeat1 (Choice [(Reference "_indStrContent"),(Reference "Interpolation"),LiteralValue]))
no constructor

_binds
(Repeat1 (Choice [(Reference "Bind"),(Reference "Inherit")]))
no constructor

Bind
(Sequence [(Reference "Attrpath"),(SyntaxValue "="),(Reference "_expr"),(SyntaxValue ";")])
Bind ExprArgument ExprArgument


Inherit
(Choice [(Sequence [(SyntaxValue "inherit"),(Reference "Attrs"),(SyntaxValue ";")]),(Sequence [(SyntaxValue "inherit"),(Reference "Parenthesized"),(Reference "Attrs"),(SyntaxValue ";")])])
Inherit ExprArgument


Attrpath
(Sequence [(Reference "_attr"),(Repeat (Sequence [(SyntaxValue "."),(Reference "_attr")]))])
Attrpath ExprArgument (ArrayArgument ExprArgument)


Attrs
(Repeat1 (Reference "_attr"))
Attrs (ArrayArgument ExprArgument)


_attr
(Choice [(Reference "Identifier"),(Reference "String"),(Reference "Interpolation")])
no constructor

Interpolation
(Sequence [(SyntaxValue "${"),(Reference "_expr"),(SyntaxValue "}")])
Interpolation ExprArgument


List
(Sequence [(SyntaxValue "["),(Repeat (Reference "_exprSelect")),(SyntaxValue "]")])
List (ArrayArgument ExprArgument)


Comment
LiteralValue
Comment StringArgument
```
