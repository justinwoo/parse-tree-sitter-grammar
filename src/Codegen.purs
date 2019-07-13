module Codegen where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Processing (RuleContent(..), RuleWithContent)

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
  | ArrayArgument Argument
derive instance genericArgument :: Generic Argument _
instance showArgument :: Show Argument where show x = genericShow x

mkExprResult :: Array RuleWithContent -> ExprResult
mkExprResult xs = { constructors }
  where
    constructors = Array.mapMaybe mkConstructor xs

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
  Repeat _ -> [ ArrayArgument ExprArgument ]
  Repeat1 _ -> [ ArrayArgument ExprArgument ]
  -- thankfully, sequences don't contain sequences
  Sequence xs -> mkArguments =<< xs
