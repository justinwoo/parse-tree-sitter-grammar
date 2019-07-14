module Output where

import Typelevel

import Data.Generic.Rep (class Generic)

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
derive instance genericExpr :: Generic Expr _

type ParseExpression = Reference "_expr"
type ParseAnon_expr = Reference "_exprFunction"
type ParseIdentifier = LiteralValue
type ParseInteger = LiteralValue
type ParseFloat = LiteralValue
type ParsePath = LiteralValue
type ParseHpath = LiteralValue
type ParseSpath = LiteralValue
type ParseUri = LiteralValue
type ParseAnon_exprFunction = Choice (Reference "Function" : Reference "Assert" : Reference "With" : Reference "Let" : Reference "_exprIf" : TypeNil)
type ParseFunction = Choice (Sequence (Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue "@" : Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Identifier" : SyntaxValue "@" : Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : TypeNil)
type ParseFormals = Choice (Sequence (SyntaxValue "{" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "," : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : TypeNil)
type ParseFormal = Sequence (Reference "Identifier" : Choice (Sequence (SyntaxValue "?" : Reference "_expr" : TypeNil) : TypeNil) : TypeNil)
type ParseEllipses = SyntaxValue "..."
type ParseAssert = Sequence (SyntaxValue "assert" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil)
type ParseWith = Sequence (SyntaxValue "with" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil)
type ParseLet = Sequence (SyntaxValue "let" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "in" : Reference "_exprFunction" : TypeNil)
type ParseAnon_exprIf = Choice (Reference "If" : Reference "_exprOp" : TypeNil)
type ParseIf = Sequence (SyntaxValue "if" : Reference "_expr" : SyntaxValue "then" : Reference "_expr" : SyntaxValue "else" : Reference "_expr" : TypeNil)
type ParseAnon_exprOp = Choice (Reference "Unary" : Reference "Binary" : Reference "_exprApp" : TypeNil)
type ParseUnary = Choice (Sequence (SyntaxValue "!" : Reference "_exprOp" : TypeNil) : Sequence (SyntaxValue "-" : Reference "_exprOp" : TypeNil) : TypeNil)
type ParseBinary = Choice (Sequence (Reference "_exprOp" : SyntaxValue "==" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "!=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "&&" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "||" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "->" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "//" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "?" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "+" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "-" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "*" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "/" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "++" : Reference "_exprOp" : TypeNil) : TypeNil)
type ParseAnon_exprApp = Choice (Reference "App" : Reference "_exprSelect" : TypeNil)
type ParseApp = Sequence (Reference "_exprApp" : Reference "_exprSelect" : TypeNil)
type ParseAnon_exprSelect = Choice (Reference "Select" : Reference "_exprSimple" : TypeNil)
type ParseSelect = Choice (Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : TypeNil) : Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : SyntaxValue "or" : Reference "_exprSelect" : TypeNil) : TypeNil)
type ParseAnon_exprSimple = Choice (Reference "Identifier" : Reference "Integer" : Reference "Float" : Reference "String" : Reference "IndentedString" : Reference "Path" : Reference "Hpath" : Reference "Spath" : Reference "Uri" : Reference "Parenthesized" : Reference "Attrset" : Reference "LetAttrset" : Reference "RecAttrset" : Reference "List" : TypeNil)
type ParseParenthesized = Sequence (SyntaxValue "(" : Reference "_expr" : SyntaxValue ")" : TypeNil)
type ParseAttrset = Sequence (SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseLetAttrset = Sequence (SyntaxValue "let" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseRecAttrset = Sequence (SyntaxValue "rec" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil)
type ParseString = LiteralValue
type IndentedString = LiteralValue
type ParseAnon_binds = Repeat1 (Choice (Reference "Bind" : Reference "Inherit" : TypeNil))
type ParseBind = Sequence (Reference "Attrpath" : SyntaxValue "=" : Reference "_expr" : SyntaxValue ";" : TypeNil)
type ParseInherit = Choice (Sequence (SyntaxValue "inherit" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : Sequence (SyntaxValue "inherit" : Reference "Parenthesized" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : TypeNil)
type ParseAttrpath = Sequence (Reference "_attr" : Repeat (Sequence (SyntaxValue "." : Reference "_attr" : TypeNil)) : TypeNil)
type ParseAttrs = Repeat1 (Reference "_attr")
type ParseAnon_attr = Choice (Reference "Identifier" : Reference "String" : Reference "Interpolation" : TypeNil)
type ParseInterpolation = Sequence (SyntaxValue "${" : Reference "_expr" : SyntaxValue "}" : TypeNil)
type ParseList = Sequence (SyntaxValue "[" : Repeat (Reference "_exprSelect") : SyntaxValue "]" : TypeNil)
type ParseComment = LiteralValue
