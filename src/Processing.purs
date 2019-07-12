module Processing where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.String (toUpper)
import Data.String.CodeUnits (singleton, toCharArray)
import Grammar (Rule, RuleType(..))

-- translated from rule type, for what the contents should be
data RuleContent
  -- just some literal content, PATTERN, TOKEN?
  = LiteralValue

  -- just some syntax value, like brackets, parens
  | SyntaxValue

  -- a reference another rule, SYMBOL
  | Reference String

  -- a choice of alternatives, CHOICE
  | Choice (Array RuleContent)

  -- a repeated rule, REPEAT
  | Repeat RuleContent

  -- a repeated1 rule, REPEAT1
  | Repeat1 RuleContent

  -- a sequence of contents, SEQUENCE
  | Sequence (Array RuleContent)

derive instance genericRuleContent :: Generic RuleContent _
instance showRuleContent :: Show RuleContent where show x = genericShow x
derive instance eqRuleContent :: Eq RuleContent
derive instance ordRuleContent :: Ord RuleContent

type RuleWithContent =
  { name :: String
  , value :: RuleContent
  }

ruleToContent :: Rule -> RuleWithContent
ruleToContent r = { name, value }
  where
    name = renameToCamelCase r.name
    value = fromRuleType r.value

fromRuleType :: RuleType -> RuleContent
fromRuleType ruleType = case ruleType of
  ALIAS -> LiteralValue
  BLANK -> LiteralValue
  CHOICE { members } -> Choice $ fromRuleType <$> members
  PATTERN _ -> LiteralValue
  PREC  { content } -> fromRuleType content
  PREC_LEFT  { content } -> fromRuleType content
  PREC_RIGHT  { content } -> fromRuleType content
  REPEAT { content } -> Repeat $ fromRuleType content
  REPEAT1 { content } -> Repeat1 $ fromRuleType content
  SEQ { members } -> Sequence $ fromRuleType <$> members
  STRING _ -> SyntaxValue
  SYMBOL { name } -> Reference $ renameToCamelCase name
  TOKEN _ -> LiteralValue

renameToCamelCase :: String -> String
renameToCamelCase str = result
  where
    list = List.fromFoldable $ map singleton $ toCharArray str
    result = case list of
      x : rest -> toUpper x <> change rest
      Nil -> ""
    change xs = case xs of
      "_" : x : rest -> toUpper x <> change rest
      x : rest -> x <> change rest
      Nil -> ""
