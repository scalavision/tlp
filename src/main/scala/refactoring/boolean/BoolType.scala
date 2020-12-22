package refactoring.boolean

object ValueLevel:

  sealed trait BoolVal:
    def not: BoolVal
    def or(that: BoolVal): BoolVal

  case object TrueVal extends BoolVal:
    def not: BoolVal = FalseVal
    def or(that: BoolVal): BoolVal = TrueVal

  case object FalseVal extends BoolVal:
    def not: BoolVal = TrueVal
    def or(that: BoolVal): BoolVal = that 

object TypeLevel2:

  sealed trait BoolType:
    type Not <: BoolType
    type Or[That <: BoolType] <: BoolType

  sealed trait TrueType extends BoolType:
    type Not = FalseType
    type Or[That <: BoolType] = TrueType

  sealed trait FalseType extends BoolType:
    type Not = TrueType
    type Or[That <: BoolType] = That

sealed trait BoolType
sealed trait TrueType extends BoolType
sealed trait FalseType extends BoolType

object BoolType:
  type \/[B1 <: BoolType, B2 <: BoolType] = Or[B1, B2]

type Not[B <: BoolType] = B match
  case TrueType => FalseType
  case FalseType => TrueType

type Or[B1 <: BoolType, B2 <: BoolType] = (B1, B2) match
  case (FalseType, FalseType) => FalseType
  case _ => TrueType

def test() =
  import BoolType._

  summon[TrueType =:= TrueType]
  summon[Not[TrueType] =:= FalseType]
  summon[Or[TrueType, FalseType] =:= TrueType]
  summon[Or[FalseType, FalseType] =:= FalseType]
  summon[FalseType \/ TrueType =:= TrueType]




  
