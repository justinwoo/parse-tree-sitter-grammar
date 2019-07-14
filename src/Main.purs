module Main where

import Prelude

import Codegen (Constructor(..), mkConstructor, mkExprResult, printExprResult)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Grammar (readRulesFromGrammarJSON')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Processing (deduplicateChoice, derefAnonymous, ruleToContent)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "grammar.json"
  rules <- readRulesFromGrammarJSON' json
  let contents = Array.mapMaybe ruleToContent rules
  traverse_ (print $ derefAnonymous contents) (deduplicateChoice <$> contents)
  traverse_ log $ printExprResult $ mkExprResult contents
  where
    print deref r = do
      log r.name
      log $ show r.value
      case mkConstructor $ deref r of
        Just ctr -> printCtr ctr
        Nothing -> log "no constructor"
      log ""
    printCtr (Constructor name args) = do
      log $ name <> " " <> intercalate " " (show <$> args)
      log ""
