module Main where

import Prelude

import Codegen (mkExprResult, printExprResult, printTypeLevel)
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Grammar (readRulesFromGrammarJSON')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Processing (ruleToContent)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "grammar.json"
  rules <- readRulesFromGrammarJSON' json
  let contents = Array.mapMaybe ruleToContent rules
  let datatype = fromMaybe "" $ printExprResult $ mkExprResult contents
  let typelevel = printTypeLevel contents
  let
    output
       = "module Generated where\n"
      <> "\n"
      <> "import Generated.Base\n"
      <> "\n"
      <> "import Data.Generic.Rep (class Generic)\n"
      <> "\n"
      <> datatype
      <> "\n"
      <> "derive instance genericExpr :: Generic Expr _\n"
      <> "\n"
      <> typelevel
      <> "\n"

  writeTextFile UTF8 "test/Generated.purs" output
