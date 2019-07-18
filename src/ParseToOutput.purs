module ParseToOutput where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))
import FormatNix (Node, children, text)
import Generated.Base (Choice, IsAnonymous, IsNamed, LiteralValue, ParseRule, Reference, Repeat, Repeat1, Sequence, Syntax(..), SyntaxValue, TLProxy(..), TypeCons, TypeNil, kind TypeList)
import GenericSumProductVariant (class GenericSumToVariant)
import Prim.Row as Row
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ParseFailure = ParseFailure String

type ParseResult a = Either (Array ParseFailure) a

class FromRule rule result | rule -> result where
  fromRule :: Proxy rule -> Node -> ParseResult result

instance fromRuleLiteral :: FromRule LiteralValue String where
  fromRule _ n = Right (text n)

instance fromRuleSyntaxValue :: FromRule SyntaxValue Syntax where
  fromRule _ n = Right $ Syntax (text n)

instance fromRuleSequence ::
  ( TupleFromSequence xs r
  ) => FromRule (Sequence xs) r where
  fromRule _ n = tupleFromSequence (TLProxy :: _ xs) 0 n

instance fromRuleChoice ::
  ( FromChoice xs result
  ) => FromRule (Choice xs) result where
  fromRule _ n = fromChoice (TLProxy :: _ xs) n

instance fromRuleRepeat ::
  ( RepeatRule x a
  ) => FromRule (Repeat x) (Array a) where
  fromRule _ n = Array.fromFoldable <$> repeatRule (Proxy :: _ x) 0 n

instance fromRuleRepeat1 ::
  FromRule (Repeat1 x) (Array a) where
  fromRule _ n = unsafeCoerce "TODO"

instance fromRuleRef ::
  FromRule Reference a where
  fromRule _ n = unsafeCoerce "TODO"

-- handle sequence to tuples, on 2+ and 2. singleton and nil not allowed.
class TupleFromSequence (xs :: TypeList) tuple | xs -> tuple where
  tupleFromSequence :: TLProxy xs -> Int -> Node -> ParseResult tuple

indexWithParseFailure :: forall a. Int -> Array a -> ParseResult a
indexWithParseFailure i xs = note error $ Array.index xs i
  where
    error = [ ParseFailure $ "Could not get item at index " <> show i ]

instance fnFromSequenceConsCons ::
  ( FromRule ruleA a
  , FromRule ruleB b
  ) => TupleFromSequence (TypeCons ruleA (TypeCons ruleB TypeNil)) (Tuple a b) where
  tupleFromSequence _ i n = Tuple <$> fa <*> fb
    where
      xs = children n
      fa = fromRule (Proxy :: _ ruleA) =<< indexWithParseFailure i xs
      fb = fromRule (Proxy :: _ ruleB) =<< indexWithParseFailure i xs

else instance fnFromSequenceCons ::
  ( TupleFromSequence tail b
  , FromRule rule a
  ) => TupleFromSequence (TypeCons rule tail) (Tuple a b) where
  tupleFromSequence _ i n = Tuple <$> fa <*> fb
    where
      xs = children n
      fa = fromRule (Proxy :: _ rule) =<< indexWithParseFailure i xs
      fb = tupleFromSequence (TLProxy :: _ tail) (i + 1) n

class FromChoice (xs :: TypeList) result | xs -> result where
  fromChoice :: TLProxy xs -> Node -> ParseResult result

instance fromChoiceFailed :: FromChoice TypeNil result where
  fromChoice _ _ = Left [ ParseFailure "Failed to parse result from choices" ]

instance fromChoiceCons ::
  ( FromRule rule result
  , FromChoice tail result
  ) => FromChoice (TypeCons rule tail) result where
  fromChoice _ n = case fromRule (Proxy :: _ rule) n of
    Right x -> Right x
    r@(Left e) -> r <|> fromChoice (TLProxy :: _ tail) n

class RepeatRule rule result | rule -> result where
  repeatRule :: Proxy rule -> Int -> Node -> ParseResult (List result)

instance repeatRuleN ::
  ( FromRule rule result
  , RepeatRule rule result
  ) => RepeatRule rule result where
  repeatRule _ i n = List.Cons <$> curr <*> rest
    where
      xs = children n
      curr = fromRule (Proxy :: _ rule) =<< indexWithParseFailure i xs
      rest = repeatRule (Proxy :: _ rule) (i + 1) n

class FromParseRule parseRule result | parseRule -> result where
  fromParseRule :: Proxy parseRule -> Node -> ParseResult result

instance fromParseRuleNamed ::
  ( FromRule rule value
  , Generic expr rep
  , Row.Cons name value row' row
  , GenericSumToVariant rep row
  ) => FromParseRule (ParseRule name IsNamed rule) expr where
  fromParseRule _ n = unsafeCoerce "TODO"

instance fromParseRuleAnon ::
  ( FromRule rule value
  ) => FromParseRule (ParseRule name IsAnonymous rule) value where
  fromParseRule _ n = unsafeCoerce "TODO"
