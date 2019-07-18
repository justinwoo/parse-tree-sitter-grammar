module ParseToOutputTest where

import Output (Expr, ParseApp, ParseAssert, ParseAttrpath, ParseAttrs, ParseAttrset, ParseBinary, ParseBind, ParseComment, ParseEllipses, ParseExpression, ParseFloat, ParseFormal, ParseFormals, ParseFunction, ParseHpath, ParseIdentifier, ParseIf, ParseIndentedString, ParseInherit, ParseInteger, ParseInterpolation, ParseLet, ParseLetAttrset, ParseParenthesized, ParsePath, ParseRecAttrset, ParseSelect, ParseSpath, ParseString, ParseUnary, ParseUri, ParseWith, ParseList)
import ParseToOutput (class FromParseRule)
import Type.Prelude (Proxy(..))

wtfIsThis :: forall parseRule fn
   . FromParseRule parseRule fn
  => Proxy parseRule -> Proxy fn
wtfIsThis _ = Proxy

testParseExpression = wtfIsThis (Proxy :: Proxy ParseExpression) :: Proxy Expr
testParseIdentifier = wtfIsThis (Proxy :: Proxy ParseIdentifier) :: Proxy Expr
testParseInteger = wtfIsThis (Proxy :: Proxy ParseInteger) :: Proxy Expr
testParseFloat = wtfIsThis (Proxy :: Proxy ParseFloat) :: Proxy Expr
testParsePath = wtfIsThis (Proxy :: Proxy ParsePath) :: Proxy Expr
testParseHpath = wtfIsThis (Proxy :: Proxy ParseHpath) :: Proxy Expr
testParseSpath = wtfIsThis (Proxy :: Proxy ParseSpath) :: Proxy Expr
testParseUri = wtfIsThis (Proxy :: Proxy ParseUri) :: Proxy Expr
testParseFunction = wtfIsThis (Proxy :: Proxy ParseFunction) :: Proxy Expr
testParseFormals = wtfIsThis (Proxy :: Proxy ParseFormals) :: Proxy Expr
testParseFormal = wtfIsThis (Proxy :: Proxy ParseFormal) :: Proxy Expr
testParseEllipses = wtfIsThis (Proxy :: Proxy ParseEllipses) :: Proxy Expr
testParseUnary = wtfIsThis (Proxy :: Proxy ParseUnary) :: Proxy Expr
testParseBinary = wtfIsThis (Proxy :: Proxy ParseBinary) :: Proxy Expr
testParseApp = wtfIsThis (Proxy :: Proxy ParseApp) :: Proxy Expr
testParseSelect = wtfIsThis (Proxy :: Proxy ParseSelect) :: Proxy Expr
testParseParenthesized = wtfIsThis (Proxy :: Proxy ParseParenthesized) :: Proxy Expr
testParseAttrset = wtfIsThis (Proxy :: Proxy ParseAttrset) :: Proxy Expr
testParseLetAttrset = wtfIsThis (Proxy :: Proxy ParseLetAttrset) :: Proxy Expr
testParseRecAttrset = wtfIsThis (Proxy :: Proxy ParseRecAttrset) :: Proxy Expr
testParseString = wtfIsThis (Proxy :: Proxy ParseString) :: Proxy Expr
testParseIndentedString = wtfIsThis (Proxy :: Proxy ParseIndentedString) :: Proxy Expr
testParseBind = wtfIsThis (Proxy :: Proxy ParseBind) :: Proxy Expr
testParseInherit = wtfIsThis (Proxy :: Proxy ParseInherit) :: Proxy Expr
testParseAttrpath = wtfIsThis (Proxy :: Proxy ParseAttrpath) :: Proxy Expr
testParseAttrs = wtfIsThis (Proxy :: Proxy ParseAttrs) :: Proxy Expr
testParseInterpolation = wtfIsThis (Proxy :: Proxy ParseInterpolation) :: Proxy Expr
testParseComment = wtfIsThis (Proxy :: Proxy ParseComment) :: Proxy Expr
testParseLet = wtfIsThis (Proxy :: Proxy ParseLet) :: Proxy Expr
testParseList = wtfIsThis (Proxy :: Proxy ParseList) :: Proxy Expr
testParseIf = wtfIsThis (Proxy :: Proxy ParseIf) :: Proxy Expr
testParseAssert = wtfIsThis (Proxy :: Proxy ParseAssert) :: Proxy Expr
