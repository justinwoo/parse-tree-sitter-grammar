module Main where

import Prelude

import Codegen (Constructor(..), mkConstructor, mkExprResult, printExprResult, printTypeLevel)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Grammar (readRulesFromGrammarJSON')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Processing (deduplicateChoice, derefAnonymous, ruleToContent)

main :: Effect Unit
main = Aff.launchAff_ do
  json <- readTextFile UTF8 "grammar.json"
  rules <- readRulesFromGrammarJSON' json
  let contents = Array.mapMaybe ruleToContent rules

  log "## Structure"
  log "```"
  traverse_ (print $ derefAnonymous contents) (deduplicateChoice <$> contents)
  log "```"
  log ""

  let datatype = fromMaybe "" $ printExprResult $ mkExprResult contents
  log "## Data type"
  log "```purs"
  log datatype
  log "```"
  log ""

  let typelevel = printTypeLevel contents
  log "## Parsing types"
  log "```purs"
  log typelevel
  log "```"

  let
    output = "module Output where\n"
          <> "\n"
          <> "import Typelevel\n"
          <> "\n"
          <> "import Data.Generic.Rep (class Generic)\n"
          <> "\n"
          <> datatype
          <> "\n"
          <> "derive instance genericExpr :: Generic Expr _\n"
          <> "\n"
          <> typelevel
          <> "\n"

  writeTextFile UTF8 "src/Output.purs" output

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
