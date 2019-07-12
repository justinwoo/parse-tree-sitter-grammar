module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Processing (renameToCamelCase)

equals :: forall a. Show a => Eq a => a -> a -> Effect Unit
equals x y
  | x == y = pure unit
  | otherwise = throwError $ error $ "not equal: " <> show { left: x, right: y }

test :: String -> Effect Unit -> Effect Unit
test label effect = do
  result <- try effect
  case result of
    Right _ -> pure unit
    Left e -> do
      log $ "Failed at test " <> label
      throwError e

main :: Effect Unit
main = do
  test "renameToCamelCase" do
    equals  "Hello" $ renameToCamelCase "hello"
    equals "HelloWorld" $ renameToCamelCase "hello_world"
    -- anonymous stays anonymous
    equals "_helloWorld" $ renameToCamelCase "_hello_world"
