module Generated.Base where

foreign import kind TypeList
foreign import data TypeCons :: Type -> TypeList -> TypeList
foreign import data TypeNil :: TypeList
infixr 5 type TypeCons as :

data TLProxy (xs :: TypeList) = TLProxy

foreign import kind Named
foreign import data IsNamed :: Named
foreign import data IsAnonymous :: Named

data LiteralValue
data SyntaxValue
data Reference
data Sequence (sequence :: TypeList)
data Choice (choices :: TypeList)
data Repeat (rule :: Type)
data Repeat1 (rule :: Type)

data ParseRule (e :: Symbol) (named :: Named) (rule :: Type) = ParseRule

newtype Syntax = Syntax String
