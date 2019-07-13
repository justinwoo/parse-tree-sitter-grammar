module Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Grammar (readRulesFromGrammarJSON')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Processing (deferenceAnonymous, ruleToContent)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "grammar.json"
  rules <- readRulesFromGrammarJSON' json
  let contents = ruleToContent <$> rules
  traverse_ (print (deferenceAnonymous contents)) contents
  where
    print deref r' = do
      -- let r = deref r'
      let r = r'
      log r.name
      log $ show r.value
      log ""
