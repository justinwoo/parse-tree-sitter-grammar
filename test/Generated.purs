module Generated where

import Generated.Base

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
  | Formal Expr Syntax Expr
  | Ellipses Syntax
  | Assert Syntax Expr Syntax Expr
  | With Syntax Expr Syntax Expr
  | Let Syntax Expr Syntax Expr
  | If Syntax Expr Syntax Expr Syntax Expr
  | Unary Syntax Expr
  | Binary Expr Syntax Expr
  | App Expr Expr
  | Select Expr
  | Parenthesized Syntax Expr Syntax
  | Attrset Syntax Expr Syntax
  | LetAttrset Syntax Syntax Expr Syntax
  | RecAttrset Syntax Syntax Expr Syntax
  | String String
  | IndentedString String
  | Binds (Array Expr)
  | Bind Expr Syntax Expr Syntax
  | Inherit Expr
  | Attrpath Expr (Array Expr)
  | Attrs (Array Expr)
  | Interpolation Syntax Expr Syntax
  | List Syntax (Array Expr) Syntax
  | Comment String
derive instance genericExpr :: Generic Expr _

type ParseExpression = ParseRule "Expression" IsNamed (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type Parse_expr = ParseRule "_expr" IsAnonymous (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseIdentifier = ParseRule "Identifier" IsNamed (LiteralValue)
type ParseInteger = ParseRule "Integer" IsNamed (LiteralValue)
type ParseFloat = ParseRule "Float" IsNamed (LiteralValue)
type ParsePath = ParseRule "Path" IsNamed (LiteralValue)
type ParseHpath = ParseRule "Hpath" IsNamed (LiteralValue)
type ParseSpath = ParseRule "Spath" IsNamed (LiteralValue)
type ParseUri = ParseRule "Uri" IsNamed (LiteralValue)
type Parse_exprFunction = ParseRule "_exprFunction" IsAnonymous (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseFunction = ParseRule "Function" IsNamed (Choice (Sequence (Reference : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : Sequence (Reference : SyntaxValue : Reference : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseFormals = ParseRule "Formals" IsNamed (Choice (Sequence (SyntaxValue : SyntaxValue : TypeNil) : Sequence (SyntaxValue : Sequence (Reference : Repeat (Sequence (SyntaxValue : Reference : TypeNil)) : TypeNil) : SyntaxValue : TypeNil) : Sequence (SyntaxValue : Sequence (Reference : Repeat (Sequence (SyntaxValue : Reference : TypeNil)) : TypeNil) : SyntaxValue : Reference : SyntaxValue : TypeNil) : Sequence (SyntaxValue : Reference : SyntaxValue : TypeNil) : TypeNil))
type ParseFormal = ParseRule "Formal" IsNamed (Sequence (Reference : Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseEllipses = ParseRule "Ellipses" IsNamed (SyntaxValue)
type ParseAssert = ParseRule "Assert" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseWith = ParseRule "With" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseLet = ParseRule "Let" IsNamed (Sequence (SyntaxValue : Reference : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type Parse_exprIf = ParseRule "_exprIf" IsAnonymous (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseIf = ParseRule "If" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type Parse_exprOp = ParseRule "_exprOp" IsAnonymous (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseUnary = ParseRule "Unary" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type ParseBinary = ParseRule "Binary" IsNamed (Sequence (Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type Parse_exprApp = ParseRule "_exprApp" IsAnonymous (Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil))
type ParseApp = ParseRule "App" IsNamed (Sequence (Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil))
type Parse_exprSelect = ParseRule "_exprSelect" IsAnonymous (Choice (Reference : Choice (Reference : TypeNil) : TypeNil))
type ParseSelect = ParseRule "Select" IsNamed (Choice (Sequence (Choice (Reference : TypeNil) : SyntaxValue : Reference : TypeNil) : Sequence (Choice (Reference : TypeNil) : SyntaxValue : Reference : SyntaxValue : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil))
type Parse_exprSimple = ParseRule "_exprSimple" IsAnonymous (Choice (Reference : TypeNil))
type ParseParenthesized = ParseRule "Parenthesized" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : TypeNil))
type ParseAttrset = ParseRule "Attrset" IsNamed (Sequence (SyntaxValue : Reference : SyntaxValue : TypeNil))
type ParseLetAttrset = ParseRule "LetAttrset" IsNamed (Sequence (SyntaxValue : SyntaxValue : Reference : SyntaxValue : TypeNil))
type ParseRecAttrset = ParseRule "RecAttrset" IsNamed (Sequence (SyntaxValue : SyntaxValue : Reference : SyntaxValue : TypeNil))
type ParseString = ParseRule "String" IsNamed LiteralValue
type ParseIndentedString = ParseRule "IndentedString" IsNamed LiteralValue
type ParseBinds = ParseRule "Binds" IsNamed (Repeat1 (Choice (Reference : TypeNil)))
type ParseBind = ParseRule "Bind" IsNamed (Sequence (Reference : SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : TypeNil))
type ParseInherit = ParseRule "Inherit" IsNamed (Choice (Sequence (SyntaxValue : Reference : SyntaxValue : TypeNil) : Sequence (SyntaxValue : Reference : Reference : SyntaxValue : TypeNil) : TypeNil))
type ParseAttrpath = ParseRule "Attrpath" IsNamed (Sequence (Choice (Reference : TypeNil) : Repeat (Sequence (SyntaxValue : Choice (Reference : TypeNil) : TypeNil)) : TypeNil))
type ParseAttrs = ParseRule "Attrs" IsNamed (Repeat1 (Choice (Reference : TypeNil)))
type Parse_attr = ParseRule "_attr" IsAnonymous (Choice (Reference : TypeNil))
type ParseInterpolation = ParseRule "Interpolation" IsNamed (Sequence (SyntaxValue : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : Choice (Reference : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : TypeNil) : SyntaxValue : TypeNil))
type ParseList = ParseRule "List" IsNamed (Sequence (SyntaxValue : Repeat (Choice (Reference : Choice (Reference : TypeNil) : TypeNil)) : SyntaxValue : TypeNil))
type ParseComment = ParseRule "Comment" IsNamed (LiteralValue)
