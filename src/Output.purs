module Output where

import Typelevel

import Data.Generic.Rep (class Generic)

newtype Syntax = Syntax String

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
  | Formal Expr Syntax Expr
  | Ellipses Syntax
  | Assert Syntax Expr Syntax Expr
  | With Syntax Expr Syntax Expr
  | Let Syntax (Array Expr) Syntax Expr
  | If Syntax Expr Syntax Expr Syntax Expr
  | Unary Expr
  | Binary Expr
  | App Expr Expr
  | Select Expr
  | Parenthesized Syntax Expr Syntax
  | Attrset Syntax (Array Expr) Syntax
  | LetAttrset Syntax Syntax (Array Expr) Syntax
  | RecAttrset Syntax Syntax (Array Expr) Syntax
  | String String
  | IndentedString String
  | Bind Expr Syntax Expr Syntax
  | Inherit Expr
  | Attrpath Expr (Array Expr)
  | Attrs (Array Expr)
  | Interpolation Syntax Expr Syntax
  | List Syntax (Array Expr) Syntax
  | Comment String
derive instance genericExpr :: Generic Expr _

type ParseExpression = ParseRule "Expression" IsNamed (Reference "_expr")
type Parse_expr = ParseRule "_expr" IsAnonymous (Reference "_exprFunction")
type ParseIdentifier = ParseRule "Identifier" IsNamed (LiteralValue)
type ParseInteger = ParseRule "Integer" IsNamed (LiteralValue)
type ParseFloat = ParseRule "Float" IsNamed (LiteralValue)
type ParsePath = ParseRule "Path" IsNamed (LiteralValue)
type ParseHpath = ParseRule "Hpath" IsNamed (LiteralValue)
type ParseSpath = ParseRule "Spath" IsNamed (LiteralValue)
type ParseUri = ParseRule "Uri" IsNamed (LiteralValue)
type Parse_exprFunction = ParseRule "_exprFunction" IsAnonymous (Choice (Reference "Function" : Reference "Assert" : Reference "With" : Reference "Let" : Reference "_exprIf" : TypeNil))
type ParseFunction = ParseRule "Function" IsNamed (Choice (Sequence (Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Formals" : SyntaxValue "@" : Reference "Identifier" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : Sequence (Reference "Identifier" : SyntaxValue "@" : Reference "Formals" : SyntaxValue ":" : Reference "_exprFunction" : TypeNil) : TypeNil))
type ParseFormals = ParseRule "Formals" IsNamed (Choice (Sequence (SyntaxValue "{" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" : TypeNil)) : TypeNil) : SyntaxValue "," : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Reference "Ellipses" : SyntaxValue "}" : TypeNil) : TypeNil))
type ParseFormal = ParseRule "Formal" IsNamed (Sequence (Reference "Identifier" : Choice (Sequence (SyntaxValue "?" : Reference "_expr" : TypeNil) : TypeNil) : TypeNil))
type ParseEllipses = ParseRule "Ellipses" IsNamed (SyntaxValue "...")
type ParseAssert = ParseRule "Assert" IsNamed (Sequence (SyntaxValue "assert" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil))
type ParseWith = ParseRule "With" IsNamed (Sequence (SyntaxValue "with" : Reference "_expr" : SyntaxValue ";" : Reference "_exprFunction" : TypeNil))
type ParseLet = ParseRule "Let" IsNamed (Sequence (SyntaxValue "let" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "in" : Reference "_exprFunction" : TypeNil))
type Parse_exprIf = ParseRule "_exprIf" IsAnonymous (Choice (Reference "If" : Reference "_exprOp" : TypeNil))
type ParseIf = ParseRule "If" IsNamed (Sequence (SyntaxValue "if" : Reference "_expr" : SyntaxValue "then" : Reference "_expr" : SyntaxValue "else" : Reference "_expr" : TypeNil))
type Parse_exprOp = ParseRule "_exprOp" IsAnonymous (Choice (Reference "Unary" : Reference "Binary" : Reference "_exprApp" : TypeNil))
type ParseUnary = ParseRule "Unary" IsNamed (Choice (Sequence (SyntaxValue "!" : Reference "_exprOp" : TypeNil) : Sequence (SyntaxValue "-" : Reference "_exprOp" : TypeNil) : TypeNil))
type ParseBinary = ParseRule "Binary" IsNamed (Choice (Sequence (Reference "_exprOp" : SyntaxValue "==" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "!=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "<=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue ">=" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "&&" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "||" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "->" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "//" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "?" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "+" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "-" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "*" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "/" : Reference "_exprOp" : TypeNil) : Sequence (Reference "_exprOp" : SyntaxValue "++" : Reference "_exprOp" : TypeNil) : TypeNil))
type Parse_exprApp = ParseRule "_exprApp" IsAnonymous (Choice (Reference "App" : Reference "_exprSelect" : TypeNil))
type ParseApp = ParseRule "App" IsNamed (Sequence (Reference "_exprApp" : Reference "_exprSelect" : TypeNil))
type Parse_exprSelect = ParseRule "_exprSelect" IsAnonymous (Choice (Reference "Select" : Reference "_exprSimple" : TypeNil))
type ParseSelect = ParseRule "Select" IsNamed (Choice (Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : TypeNil) : Sequence (Reference "_exprSimple" : SyntaxValue "." : Reference "Attrpath" : SyntaxValue "or" : Reference "_exprSelect" : TypeNil) : TypeNil))
type Parse_exprSimple = ParseRule "_exprSimple" IsAnonymous (Choice (Reference "Identifier" : Reference "Integer" : Reference "Float" : Reference "String" : Reference "IndentedString" : Reference "Path" : Reference "Hpath" : Reference "Spath" : Reference "Uri" : Reference "Parenthesized" : Reference "Attrset" : Reference "LetAttrset" : Reference "RecAttrset" : Reference "List" : TypeNil))
type ParseParenthesized = ParseRule "Parenthesized" IsNamed (Sequence (SyntaxValue "(" : Reference "_expr" : SyntaxValue ")" : TypeNil))
type ParseAttrset = ParseRule "Attrset" IsNamed (Sequence (SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseLetAttrset = ParseRule "LetAttrset" IsNamed (Sequence (SyntaxValue "let" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseRecAttrset = ParseRule "RecAttrset" IsNamed (Sequence (SyntaxValue "rec" : SyntaxValue "{" : Choice (Reference "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseString = ParseRule "String" IsNamed LiteralValue
type ParseIndentedString = ParseRule "IndentedString" IsNamed LiteralValue
type Parse_binds = ParseRule "_binds" IsAnonymous (Repeat1 (Choice (Reference "Bind" : Reference "Inherit" : TypeNil)))
type ParseBind = ParseRule "Bind" IsNamed (Sequence (Reference "Attrpath" : SyntaxValue "=" : Reference "_expr" : SyntaxValue ";" : TypeNil))
type ParseInherit = ParseRule "Inherit" IsNamed (Choice (Sequence (SyntaxValue "inherit" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : Sequence (SyntaxValue "inherit" : Reference "Parenthesized" : Reference "Attrs" : SyntaxValue ";" : TypeNil) : TypeNil))
type ParseAttrpath = ParseRule "Attrpath" IsNamed (Sequence (Reference "_attr" : Repeat (Sequence (SyntaxValue "." : Reference "_attr" : TypeNil)) : TypeNil))
type ParseAttrs = ParseRule "Attrs" IsNamed (Repeat1 (Reference "_attr"))
type Parse_attr = ParseRule "_attr" IsAnonymous (Choice (Reference "Identifier" : Reference "String" : Reference "Interpolation" : TypeNil))
type ParseInterpolation = ParseRule "Interpolation" IsNamed (Sequence (SyntaxValue "${" : Reference "_expr" : SyntaxValue "}" : TypeNil))
type ParseList = ParseRule "List" IsNamed (Sequence (SyntaxValue "[" : Repeat (Reference "_exprSelect") : SyntaxValue "]" : TypeNil))
type ParseComment = ParseRule "Comment" IsNamed (LiteralValue)
