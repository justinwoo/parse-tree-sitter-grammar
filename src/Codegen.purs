module Codegen where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Processing (RuleContent(..), RuleWithContent, processRulesWithContent)

-- goal: make data Expr of constructors, basically
type ExprResult =
  { constructors :: Array Constructor
  }

-- constructor of expr
data Constructor = Constructor Name (Array Argument)
type Name = String

data Argument
  = StringArgument
  | ExprArgument
  | ArrayExprArgument
  | SyntaxArgument
derive instance genericArgument :: Generic Argument _
instance showArgument :: Show Argument where show x = genericShow x

printExprResult :: ExprResult -> Maybe String
printExprResult { constructors } = case Array.uncons constructors of
  Nothing -> Nothing
  Just { head, tail } -> Just $ "data Expr" <> first <> rest
    where
      first = "\n  = " <> printConstructor head
      printRest item = "\n  | " <> printConstructor item
      rest = Array.foldMap printRest tail
      printConstructor (Constructor name args) = case name of
        -- special case strings because i dont care about the contents
        "String" -> name <> " String"
        "IndentedString" -> name <> " String"
        _ -> name <> Array.foldMap printArgument args
      printArgument arg = case arg of
        StringArgument -> " String"
        ExprArgument -> " Expr"
        ArrayExprArgument -> " (Array Expr)"
        SyntaxArgument  -> " Syntax"

mkExprResult :: Array RuleWithContent -> ExprResult
mkExprResult xs = { constructors }
  where
    ys = processRulesWithContent xs
    constructors = Array.mapMaybe mkConstructor ys

-- only non anonymous can have constructors
mkConstructor :: RuleWithContent -> Maybe Constructor
mkConstructor { isAnonymous: true } = Nothing
mkConstructor r = Just $ Constructor r.name $ mkArguments r.value

-- what arguments does the constructor take for these
mkArguments :: RuleContent -> Array Argument
mkArguments r = case r of
  LiteralValue -> [ StringArgument ]
  SyntaxValue -> [ SyntaxArgument ]
  Reference _ -> [ ExprArgument ]
  Choice _ -> [ ExprArgument ]
  Repeat _ -> [ ArrayExprArgument ]
  Repeat1 _ -> [ ArrayExprArgument ]
  -- thankfully, sequences don't contain sequences
  Sequence xs -> mkArguments =<< xs

data TypeLevelCodegen
  = TLLiteralValue
  | TLSyntaxValue
  | TLReference
  | TLSequence (Array TypeLevelCodegen)
  | TLChoice (Array TypeLevelCodegen)
  | TLRepeat TypeLevelCodegen
  | TLRepeat1 TypeLevelCodegen

derive instance eqTypeLevelCodegen :: Eq TypeLevelCodegen
derive instance ordTypeLevelCodegen :: Ord TypeLevelCodegen

ruleContentToTLCodegen :: RuleContent -> TypeLevelCodegen
ruleContentToTLCodegen rc = case rc of
  LiteralValue -> TLLiteralValue
  SyntaxValue -> TLSyntaxValue
  Reference _ -> TLReference
  Choice xs -> TLChoice $ Array.nub $ map ruleContentToTLCodegen xs
  Sequence xs -> TLSequence $ map ruleContentToTLCodegen xs
  Repeat x -> TLRepeat $ ruleContentToTLCodegen x
  Repeat1 x -> TLRepeat1 $ ruleContentToTLCodegen x

printTypeLevel :: Array RuleWithContent -> String
printTypeLevel rules' =
  Array.intercalate "\n" $ Array.mapMaybe printRule rules
  where
    rules = processRulesWithContent rules'

    -- special case strings, which i dont care about
    printRule { name: "String" } = Just $
      "type ParseString = ParseRule " <> quoted "String" <> " IsNamed LiteralValue"
    printRule { name: "IndentedString" } = Just $
      "type ParseIndentedString = ParseRule " <> quoted "IndentedString" <> " IsNamed LiteralValue"
    printRule { name: "_stringParts" } = Nothing
    printRule { name: "_indStringParts" } = Nothing
    printRule { name, isAnonymous, value } = Just do
      let flag = if not isAnonymous then "IsNamed" else "IsAnonymous"
      "type Parse" <> name <> " = ParseRule " <> quoted name <> " " <> flag <> " (" <> print value <> ")"

    print :: RuleContent -> String
    print r = print' $ ruleContentToTLCodegen r

    print' :: TypeLevelCodegen -> String
    print' r = case r of
      TLLiteralValue -> "LiteralValue"
      TLSyntaxValue -> "SyntaxValue"
      TLReference -> "Reference"
      TLChoice xs -> "Choice (" <> list xs <> ")"
      TLRepeat x -> "Repeat (" <> print' x <> ")"
      TLRepeat1 x -> "Repeat1 (" <> print' x <> ")"
      TLSequence xs -> "Sequence (" <> list xs <> ")"

    quoted s = "\"" <> s <> "\""
    item x = print' x <> " : "
    list xs = Array.foldMap item xs <> "TypeNil"
