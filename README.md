# trash

i might work on this later, we'll see

## Structure
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
(Sequence [(Reference "Identifier"),(Sequence [(SyntaxValue "?"),(Reference "_expr")])])
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
(Sequence [(SyntaxValue "let"),(Reference "_binds"),(SyntaxValue "in"),(Reference "_exprFunction")])
Let ArrayExprArgument ExprArgument


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
(Sequence [(SyntaxValue "{"),(Reference "_binds"),(SyntaxValue "}")])
Attrset ArrayExprArgument


LetAttrset
(Sequence [(SyntaxValue "let"),(SyntaxValue "{"),(Reference "_binds"),(SyntaxValue "}")])
LetAttrset ArrayExprArgument


RecAttrset
(Sequence [(SyntaxValue "rec"),(SyntaxValue "{"),(Reference "_binds"),(SyntaxValue "}")])
RecAttrset ArrayExprArgument


String
(Sequence [(SyntaxValue "\""),(Reference "_stringParts"),(SyntaxValue "\"")])
String ArrayExprArgument


IndentedString
(Sequence [(SyntaxValue "''"),(Reference "_indStringParts"),(SyntaxValue "''")])
IndentedString ArrayExprArgument


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
Attrpath ExprArgument ArrayExprArgument


Attrs
(Repeat1 (Reference "_attr"))
Attrs ArrayExprArgument


_attr
(Choice [(Reference "Identifier"),(Reference "String"),(Reference "Interpolation")])
no constructor

Interpolation
(Sequence [(SyntaxValue "${"),(Reference "_expr"),(SyntaxValue "}")])
Interpolation ExprArgument


List
(Sequence [(SyntaxValue "["),(Repeat (Reference "_exprSelect")),(SyntaxValue "]")])
List ArrayExprArgument


Comment
LiteralValue
Comment StringArgument


```

## Data type
```purs
data Expr
  = Expression Expr
  | Identifier String
  | Integer String
  | Float String
  | Path String
  | Hpath String
  | Spath String
  | Uri String
  | Function Expr
  | Formals Expr
  | Formal Expr Expr
  | Ellipses
  | Assert Expr Expr
  | With Expr Expr
  | Let (Array Expr) Expr
  | If Expr Expr Expr
  | Unary Expr
  | Binary Expr
  | App Expr Expr
  | Select Expr
  | Parenthesized Expr
  | Attrset (Array Expr)
  | LetAttrset (Array Expr)
  | RecAttrset (Array Expr)
  | String String
  | IndentedString String
  | Bind Expr Expr
  | Inherit Expr
  | Attrpath Expr (Array Expr)
  | Attrs (Array Expr)
  | Interpolation Expr
  | List (Array Expr)
  | Comment String
```

## Parsing types
```purs
type ParseExpression = Reference "_expr"
type Parse_expr = Reference "_exprFunction"
type ParseIdentifier = LiteralValue
type ParseInteger = LiteralValue
type ParseFloat = LiteralValue
type ParsePath = LiteralValue
type ParseHpath = LiteralValue
type ParseSpath = LiteralValue
type ParseUri = LiteralValue
type Parse_exprFunction = Choice (Reference "Function" : Reference "Assert" : Reference "With" : Reference "Let" : Reference "_exprIf" : TypeNil)
type ParseFunction = Choice (Sequence (Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue "@" : Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Identifier" : SyntaxValue "@" : Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : TypeNil)
type ParseFormals = Choice (Sequence (SyntaxValue "{" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "," : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : TypeNil)
type ParseFormal = Sequence (Reference "Identifier" : Choice (Sequence (SyntaxValue "?" : Reference "_expr" : TypeNil) : TypeNil) : TypeNil)
type ParseEllipses = SyntaxValue "..."
type ParseAssert = Sequence (SyntaxValue "assert" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil)
type ParseWith = Sequence (SyntaxValue "with" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil)
type ParseLet = Sequence (SyntaxValue "let" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "in" : Reference "_exprFunction" : TypeNil)
type Parse_exprIf = Choice (Reference "If" : Reference "_exprOp" : TypeNil)
type ParseIf = Sequence (SyntaxValue "if" : Reference "_expr" : SyntaxValue "then" : Reference "_expr" : SyntaxValue "else" : Reference "_expr" : TypeNil)
type Parse_exprOp = Choice (Reference "Unary" : Reference "Binary" : Reference "_exprApp" : TypeNil)
type ParseUnary = Choice (Sequence (SyntaxValue "!" : Reference "_exprOp" : TypeNil) : Sequence (SyntaxValue "-" : Reference "_exprOp" : TypeNil) : TypeNil)
type ParseBinary = Choice (Sequence (Reference "_exprOp" : SyntaxValue "==" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "!=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "&&" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "||" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "->" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "//" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "?" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "+" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "-" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "*" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "/" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "++" : Reference "_exprOp" : TypeNil) : TypeNil)
type Parse_exprApp = Choice (Reference "App" : Reference "_exprSelect" : TypeNil)
type ParseApp = Sequence (Reference "_exprApp" : Reference "_exprSelect" : TypeNil)
type Parse_exprSelect = Choice (Reference "Select" : Reference "_exprSimple" : TypeNil)
type ParseSelect = Choice (Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : TypeNil) : Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : SyntaxValue "or" : Reference "_exprSelect" : TypeNil) : TypeNil)
type Parse_exprSimple = Choice (Reference "Identifier" : Reference "Integer" : Reference "Float" : Reference "String" : Reference "IndentedString" : Reference "Path" : Reference "Hpath" : Reference "Spath" : Reference "Uri" : Reference "Parenthesized" : Reference "Attrset" : Reference "LetAttrset" : Reference "RecAttrset" : Reference "List" : TypeNil)
type ParseParenthesized = Sequence (SyntaxValue "(" : Reference "_expr" : SyntaxValue ")" : TypeNil)
type ParseAttrset = Sequence (SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseLetAttrset = Sequence (SyntaxValue "let" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseRecAttrset = Sequence (SyntaxValue "rec" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseString = LiteralValue
type IndentedString = LiteralValue
type Parse_binds = Repeat1 (Choice (Reference "Bind" : Reference "Inherit" : TypeNil))
type ParseBind = Sequence (Reference "Attrpath" : SyntaxValue "=" : Reference "_expr" : SyntaxValue ";" : TypeNil)
type ParseInherit = Choice (Sequence (SyntaxValue "inherit" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : Sequence (SyntaxValue "inherit" : Reference "Parenthesized" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : TypeNil)
type ParseAttrpath = Sequence (Reference "_attr" : Repeat (Sequence (SyntaxValue "." : Reference "_attr" : TypeNil)) : TypeNil)
type ParseAttrs = Repeat1 (Reference "_attr")
type Parse_attr = Choice (Reference "Identifier" : Reference "String" : Reference "Interpolation" : TypeNil)
type ParseInterpolation = Sequence (SyntaxValue "${" : Reference "_expr" : SyntaxValue "}" : TypeNil)
type ParseList = Sequence (SyntaxValue "[" : Repeat (Reference "_exprSelect") : SyntaxValue "]" : TypeNil)
type ParseComment = LiteralValue
```
