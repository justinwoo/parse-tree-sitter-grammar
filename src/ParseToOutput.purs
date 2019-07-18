module ParseToOutput where

import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple)
import GenericSumProductVariant (class GenericSumToVariant)
import Prim.Row as Row
import Type.Prelude (class IsSymbol)
import Typelevel (Choice, IsAnonymous, IsNamed, LiteralValue, ParseRule, Reference, Repeat, Repeat1, Sequence, Syntax, SyntaxValue, TypeCons, TypeNil, kind TypeList)

class FromRule rule result | rule -> result

instance fromRuleLiteral :: FromRule LiteralValue String

instance fromRuleSyntaxValue ::
  ( IsSymbol literal
  ) => FromRule (SyntaxValue literal) Syntax

instance fromRuleSequence ::
  ( TupleFromSequence xs r
  ) => FromRule (Sequence xs) r

instance fromRuleChoice ::
  FromRule (Choice xs) a

instance fromRuleRepeat ::
  FromRule (Repeat xs) (Array a)

instance fromRuleRepeat1 ::
  FromRule (Repeat1 xs) (Array a)

instance fromRuleRef ::
  FromRule (Reference name typeName) a

-- handle sequence to tuples, on 2+ and 2. singleton and nil not allowed.
class TupleFromSequence (xs :: TypeList) tuple | xs -> tuple

instance fnFromSequenceConsCons ::
  ( FromRule rule a
  , FromRule rule2 b
  ) => TupleFromSequence (TypeCons rule (TypeCons rule2 TypeNil)) (Tuple a b)
else instance fnFromSequenceCons ::
  ( TupleFromSequence tail b
  , FromRule rule a
  ) => TupleFromSequence (TypeCons rule tail) (Tuple a b)


class FromParseRule parseRule result | parseRule -> result

instance fromParseRuleNamed ::
  ( FromRule rule value
  , Generic expr rep
  , Row.Cons name value row' row
  , GenericSumToVariant rep row
  ) => FromParseRule (ParseRule name IsNamed rule) expr

instance fromParseRuleAnon ::
  ( FromRule rule value
  ) => FromParseRule (ParseRule name IsAnonymous rule) value
