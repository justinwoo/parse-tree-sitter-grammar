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

type ParseExpression = ParseRule "Expression" IsNamed (Reference "_expr" "_expr")
type Parse_expr = ParseRule "_expr" IsAnonymous (Reference "_exprFunction" "_expr_function")
type ParseIdentifier = ParseRule "Identifier" IsNamed (LiteralValue)
type ParseInteger = ParseRule "Integer" IsNamed (LiteralValue)
type ParseFloat = ParseRule "Float" IsNamed (LiteralValue)
type ParsePath = ParseRule "Path" IsNamed (LiteralValue)
type ParseHpath = ParseRule "Hpath" IsNamed (LiteralValue)
type ParseSpath = ParseRule "Spath" IsNamed (LiteralValue)
type ParseUri = ParseRule "Uri" IsNamed (LiteralValue)
type Parse_exprFunction = ParseRule "_exprFunction" IsAnonymous (Choice (Reference "Function" "function" : Reference "Assert" "assert" : Reference "With" "with" : Reference "Let" "let" : Reference "_exprIf" "_expr_if" : TypeNil))
type ParseFunction = ParseRule "Function" IsNamed (Choice (Sequence (Reference "Identifier" "identifier" : SyntaxValue ":" : Reference "_exprFunction" "_expr_function" : TypeNil) : Sequence (Reference "Formals" "formals" : SyntaxValue ":" : Reference "_exprFunction" "_expr_function" : TypeNil) : Sequence (Reference "Formals" "formals" : SyntaxValue "@" : Reference "Identifier" "identifier" : SyntaxValue ":" : Reference "_exprFunction" "_expr_function" : TypeNil) : Sequence (Reference "Identifier" "identifier" : SyntaxValue "@" : Reference "Formals" "formals" : SyntaxValue ":" : Reference "_exprFunction" "_expr_function" : TypeNil) : TypeNil))
type ParseFormals = ParseRule "Formals" IsNamed (Choice (Sequence (SyntaxValue "{" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" "formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" "formal" : TypeNil)) : TypeNil) : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Sequence (Reference "Formal" "formal" : Repeat (Sequence (SyntaxValue "," : Reference "Formal" "formal" : TypeNil)) : TypeNil) : SyntaxValue "," : Reference "Ellipses" "ellipses" : SyntaxValue "}" : TypeNil) : Sequence (SyntaxValue "{" : Reference "Ellipses" "ellipses" : SyntaxValue "}" : TypeNil) : TypeNil))
type ParseFormal = ParseRule "Formal" IsNamed (Sequence (Reference "Identifier" "identifier" : Choice (Sequence (SyntaxValue "?" : Reference "_expr" "_expr" : TypeNil) : TypeNil) : TypeNil))
type ParseEllipses = ParseRule "Ellipses" IsNamed (SyntaxValue "...")
type ParseAssert = ParseRule "Assert" IsNamed (Sequence (SyntaxValue "assert" : Reference "_expr" "_expr" : SyntaxValue ";" : Reference "_exprFunction" "_expr_function" : TypeNil))
type ParseWith = ParseRule "With" IsNamed (Sequence (SyntaxValue "with" : Reference "_expr" "_expr" : SyntaxValue ";" : Reference "_exprFunction" "_expr_function" : TypeNil))
type ParseLet = ParseRule "Let" IsNamed (Sequence (SyntaxValue "let" : Choice (Reference "_binds" "_binds" : TypeNil) : SyntaxValue "in" : Reference "_exprFunction" "_expr_function" : TypeNil))
type Parse_exprIf = ParseRule "_exprIf" IsAnonymous (Choice (Reference "If" "if" : Reference "_exprOp" "_expr_op" : TypeNil))
type ParseIf = ParseRule "If" IsNamed (Sequence (SyntaxValue "if" : Reference "_expr" "_expr" : SyntaxValue "then" : Reference "_expr" "_expr" : SyntaxValue "else" : Reference "_expr" "_expr" : TypeNil))
type Parse_exprOp = ParseRule "_exprOp" IsAnonymous (Choice (Reference "Unary" "unary" : Reference "Binary" "binary" : Reference "_exprApp" "_expr_app" : TypeNil))
type ParseUnary = ParseRule "Unary" IsNamed (Choice (Sequence (SyntaxValue "!" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (SyntaxValue "-" : Reference "_exprOp" "_expr_op" : TypeNil) : TypeNil))
type ParseBinary = ParseRule "Binary" IsNamed (Choice (Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "==" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "!=" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "<" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "<=" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue ">" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue ">=" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "&&" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "||" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "->" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "//" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "?" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "+" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "-" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "*" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "/" : Reference "_exprOp" "_expr_op" : TypeNil) : Sequence (Reference "_exprOp" "_expr_op" : SyntaxValue "++" : Reference "_exprOp" "_expr_op" : TypeNil) : TypeNil))
type Parse_exprApp = ParseRule "_exprApp" IsAnonymous (Choice (Reference "App" "app" : Reference "_exprSelect" "_expr_select" : TypeNil))
type ParseApp = ParseRule "App" IsNamed (Sequence (Reference "_exprApp" "_expr_app" : Reference "_exprSelect" "_expr_select" : TypeNil))
type Parse_exprSelect = ParseRule "_exprSelect" IsAnonymous (Choice (Reference "Select" "select" : Reference "_exprSimple" "_expr_simple" : TypeNil))
type ParseSelect = ParseRule "Select" IsNamed (Choice (Sequence (Reference "_exprSimple" "_expr_simple" : SyntaxValue "." : Reference "Attrpath" "attrpath" : TypeNil) : Sequence (Reference "_exprSimple" "_expr_simple" : SyntaxValue "." : Reference "Attrpath" "attrpath" : SyntaxValue "or" : Reference "_exprSelect" "_expr_select" : TypeNil) : TypeNil))
type Parse_exprSimple = ParseRule "_exprSimple" IsAnonymous (Choice (Reference "Identifier" "identifier" : Reference "Integer" "integer" : Reference "Float" "float" : Reference "String" "string" : Reference "IndentedString" "indented_string" : Reference "Path" "path" : Reference "Hpath" "hpath" : Reference "Spath" "spath" : Reference "Uri" "uri" : Reference "Parenthesized" "parenthesized" : Reference "Attrset" "attrset" : Reference "LetAttrset" "let_attrset" : Reference "RecAttrset" "rec_attrset" : Reference "List" "list" : TypeNil))
type ParseParenthesized = ParseRule "Parenthesized" IsNamed (Sequence (SyntaxValue "(" : Reference "_expr" "_expr" : SyntaxValue ")" : TypeNil))
type ParseAttrset = ParseRule "Attrset" IsNamed (Sequence (SyntaxValue "{" : Choice (Reference "_binds" "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseLetAttrset = ParseRule "LetAttrset" IsNamed (Sequence (SyntaxValue "let" : SyntaxValue "{" : Choice (Reference "_binds" "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseRecAttrset = ParseRule "RecAttrset" IsNamed (Sequence (SyntaxValue "rec" : SyntaxValue "{" : Choice (Reference "_binds" "_binds" : TypeNil) : SyntaxValue "}" : TypeNil))
type ParseString = ParseRule "String" IsNamed LiteralValue
type ParseIndentedString = ParseRule "IndentedString" IsNamed LiteralValue
type Parse_binds = ParseRule "_binds" IsAnonymous (Repeat1 (Choice (Reference "Bind" "bind" : Reference "Inherit" "inherit" : TypeNil)))
type ParseBind = ParseRule "Bind" IsNamed (Sequence (Reference "Attrpath" "attrpath" : SyntaxValue "=" : Reference "_expr" "_expr" : SyntaxValue ";" : TypeNil))
type ParseInherit = ParseRule "Inherit" IsNamed (Choice (Sequence (SyntaxValue "inherit" : Reference "Attrs" "attrs" : SyntaxValue ";" : TypeNil) : Sequence (SyntaxValue "inherit" : Reference "Parenthesized" "parenthesized" : Reference "Attrs" "attrs" : SyntaxValue ";" : TypeNil) : TypeNil))
type ParseAttrpath = ParseRule "Attrpath" IsNamed (Sequence (Reference "_attr" "_attr" : Repeat (Sequence (SyntaxValue "." : Reference "_attr" "_attr" : TypeNil)) : TypeNil))
type ParseAttrs = ParseRule "Attrs" IsNamed (Repeat1 (Reference "_attr" "_attr"))
type Parse_attr = ParseRule "_attr" IsAnonymous (Choice (Reference "Identifier" "identifier" : Reference "String" "string" : Reference "Interpolation" "interpolation" : TypeNil))
type ParseInterpolation = ParseRule "Interpolation" IsNamed (Sequence (SyntaxValue "${" : Reference "_expr" "_expr" : SyntaxValue "}" : TypeNil))
type ParseList = ParseRule "List" IsNamed (Sequence (SyntaxValue "[" : Repeat (Reference "_exprSelect" "_expr_select") : SyntaxValue "]" : TypeNil))
type ParseComment = ParseRule "Comment" IsNamed (LiteralValue)
