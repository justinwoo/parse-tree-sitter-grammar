module Codegen where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Processing (RuleContent(..), RuleWithContent, deduplicateChoice, deferenceAnonymous)

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
      printConstructor (Constructor name args) =
        name <> Array.foldMap printArgument args
      printArgument arg = case arg of
        StringArgument -> " String"
        ExprArgument -> " Expr"
        ArrayExprArgument -> " (Array Expr)"

mkExprResult :: Array RuleWithContent -> ExprResult
mkExprResult xs = { constructors }
  where
    ys = deduplicateChoice <<< deferenceAnonymous xs <$> xs
    constructors = Array.mapMaybe mkConstructor ys

-- only non anonymous can have constructors
mkConstructor :: RuleWithContent -> Maybe Constructor
mkConstructor { isAnonymous: true } = Nothing
mkConstructor r = Just $ Constructor r.name $ mkArguments r.value

-- what arguments does the constructor take for these
mkArguments :: RuleContent -> Array Argument
mkArguments r = case r of
  LiteralValue -> [ StringArgument ]
  SyntaxValue _ -> []
  Reference _ -> [ ExprArgument ]
  Choice _ -> [ ExprArgument ]
  Repeat _ -> [ ArrayExprArgument ]
  Repeat1 _ -> [ ArrayExprArgument ]
  -- thankfully, sequences don't contain sequences
  Sequence xs -> mkArguments =<< xs
