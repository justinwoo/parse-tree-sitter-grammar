module Processing where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.CodeUnits (stripPrefix) as String
import Data.Traversable (traverse)
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

immediate :: forall f. Applicative f => (RuleContent -> f RuleContent) -> RuleContent -> f RuleContent
immediate f r = case r of
  LiteralValue -> pure LiteralValue
  SyntaxValue -> pure SyntaxValue
  Reference string -> pure $ Reference string
  Choice xs -> Choice <$> traverse f xs
  Repeat x -> Repeat <$> f x
  Repeat1 x -> Repeat1 <$> f x
  Sequence xs -> Sequence <$> traverse f xs

type RuleWithContent =
  { name :: String
  , value :: RuleContent
  -- the name begins with _, so it is not a real node
  , isAnonymous :: Boolean
  }

ruleToContent :: Rule -> RuleWithContent
ruleToContent r = { name, value, isAnonymous }
  where
    name = renameToCamelCase r.name
    value = fromRuleType r.value
    isAnonymous = isJust $ String.stripPrefix (Pattern "_") name

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

-- because unary and binary have 20 cases, of which none matter
deduplicateChoice :: RuleWithContent -> RuleWithContent
deduplicateChoice rwc = rwc { value = deduplicate rwc.value }
  where
    deduplicate r = un Identity case r of
      Choice choices -> pure $ Choice $ Array.nub choices
      _ -> immediate (pure <<< deduplicate) r

deferenceAnonymous :: Array RuleWithContent -> RuleWithContent -> RuleWithContent
deferenceAnonymous xs rwc = rwc { value = deref rwc.value }
  where
    deref :: RuleContent -> RuleContent
    deref r = un Identity case r of
      Reference name
        | Just x <- Array.find (eq name <<< _.name) xs
        , x.isAnonymous -> pure $ deref x.value
      _ -> immediate (pure <<< deref) r

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
