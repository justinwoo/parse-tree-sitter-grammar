module Grammar where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Foreign (ForeignError(..))
import Foreign.Object (Object)
import Foreign.Object as FO
import Simple.JSON as JSON
import Simple.JSON.Utils (printMultipleErrors)

type Rule =
  { name :: String
  , value :: RuleType
  }

-- a rule type in the grammar, which is under the "type" property
-- additional properties are attached based on the type involved
data RuleType
  -- alias? part of some indented string parts
  = ALIAS

  -- apparently just a blank space or something
  | BLANK

  -- a choice between other rules to apply
  | CHOICE { members :: Array RuleType }

  -- some string pattern, value is a regex string
  | PATTERN { value :: String }

  -- precedence rules. dont know if i care too much
  | PREC { value :: Int, content :: RuleType }
  | PREC_LEFT { value :: Int, content :: RuleType }
  | PREC_RIGHT { value :: Int, content :: RuleType }

  -- a repeat rule for zero or more times
  | REPEAT { content :: RuleType }

  -- a repeat rule for one or more times
  | REPEAT1 { content :: RuleType }

  -- a sequence of rules to be applied in order
  | SEQ { members :: Array RuleType }

  -- a literal string character
  | STRING { value :: String }

  -- a defined rule to be applied, generally the most interesting.
  -- e.g.
  -- "interpolation": {
  --   "type": "SEQ",
  --   "members": [
  --     {"type": "STRING", "value": "${"},
  --     {"type": "SYMBOL", "name": "_expr"},
  --     {"type": "STRING", "value": "}"}
  --   ]
  -- },
  | SYMBOL { name :: String }

  -- apparently this is just only used for comment
  | TOKEN { content :: RuleType }

derive instance genericRuleType :: Generic RuleType _
instance showRuleType :: Show RuleType where show x = genericShow x

instance readJSONRuleType :: JSON.ReadForeign RuleType where
  readImpl f = do
    r :: { "type" :: String } <- JSON.readImpl f
    case r."type" of
      "ALIAS" -> pure ALIAS
      "BLANK" -> pure BLANK
      "CHOICE" -> CHOICE <$> JSON.readImpl f
      "PATTERN" -> PATTERN <$> JSON.readImpl f
      "PREC" -> PREC <$> JSON.readImpl f
      "PREC_LEFT" -> PREC_LEFT <$> JSON.readImpl f
      "PREC_RIGHT" -> PREC_RIGHT <$> JSON.readImpl f
      "REPEAT" -> REPEAT <$> JSON.readImpl f
      "REPEAT1" -> REPEAT1 <$> JSON.readImpl f
      "SEQ" -> SEQ <$> JSON.readImpl f
      "STRING" -> STRING <$> JSON.readImpl f
      "SYMBOL" -> SYMBOL <$> JSON.readImpl f
      "TOKEN" -> TOKEN <$> JSON.readImpl f
      x -> throwError $ pure $ ForeignError $
        "Cannot decode rule of type " <> x

readRulesFromGrammarJSON :: String -> JSON.E (Array Rule)
readRulesFromGrammarJSON json = do
  r :: { rules :: Object RuleType } <- JSON.readJSON json
  pure $ fromPair <$> FO.toUnfoldable r.rules
  where
    fromPair (Tuple name value) = { name, value }

readRulesFromGrammarJSON' :: String -> Aff (Array Rule)
readRulesFromGrammarJSON' json =
  case readRulesFromGrammarJSON json of
    Left e -> throwError $ Aff.error $ printMultipleErrors e
    Right x -> pure x
