module Main where

import Prelude

import Codegen (Constructor(..), mkConstructor)
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Grammar (readRulesFromGrammarJSON')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Processing (deduplicateChoice, deferenceAnonymous, ruleToContent)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "grammar.json"
  rules <- readRulesFromGrammarJSON' json
  let contents = deduplicateChoice <<< ruleToContent <$> rules
  traverse_ (print (deferenceAnonymous contents)) contents
  where
    print deref r = do
      log r.name
      log $ show r.value
      case mkConstructor r of
        Just ctr -> printCtr ctr
        Nothing -> log "no constructor"
      log ""
    printCtr (Constructor name args) = do
      log $ name <> " " <> intercalate " " (show <$> args)
      log ""
