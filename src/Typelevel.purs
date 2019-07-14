module Typelevel where

foreign import kind TypeList
foreign import data TypeCons :: Type -> TypeList -> TypeList
foreign import data TypeNil :: TypeList
infixr 5 type TypeCons as :

foreign import kind Named
foreign import data IsNamed :: Named
foreign import data IsAnonymous :: Named

data LiteralValue
data SyntaxValue (literal :: Symbol)
data Reference (name :: Symbol)
data Sequence (sequence :: TypeList)
data Choice (choices :: TypeList)
data Repeat (rule :: Type)
data Repeat1 (rule :: Type)

data ParseRule (named :: Named) (rule :: Type) = ParseRule
