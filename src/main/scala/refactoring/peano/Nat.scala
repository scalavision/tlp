package refactoring.peano

object ValueLevel:
  
  sealed trait Nat:
    def +(that: Nat): Nat

  case object Nat0 extends Nat:
    override def +(that: Nat) = that

  case class NatN(prev: Nat) extends Nat:
    override def +(that: Nat) = NatN(prev + that)


object TypeLevel2:
  
  sealed trait Nat:
    type plus[That <: Nat] <: Nat

  sealed trait Zero extends Nat:
    type plus[That <: Nat] = That

  sealed trait NatN[N <: Nat] extends Nat
    // # does not work in Scala 3
    // type plus[That <: Nat] = NatN[N#plus[That]]

  object Nat
    // # does not work in Scala 3
    // type +[A <: Nat, B <: Nat] = A#plus[B]

sealed trait Nat
sealed trait Zero extends Nat
sealed trait Succ[N <: Nat] extends Nat

import scala.compiletime._

type Dec[X1 <: Nat] <: Nat = X1 match
  case Succ[n] => n

type Inc[X1 <: Nat] <: Nat = X1 match
  case Zero => Succ[Zero]
  case Succ[n] => Succ[Inc[n]]

type Plus[X1 <: Nat, X2 <: Nat] <: Nat = X1 match
  case Zero => X2
  case Succ[n] => Succ[Plus[n, X2]]

type ToInt[X <: Nat] <: Int = X match
  case Zero => 0
  case Succ[n] => S[ToInt[n]]

object Nat:

  type +[N1 <: Nat, N2 <: Nat] = Plus[N1, N2]

  type One = Succ[Zero]
  type Two = Succ[One]
  type Three = Succ[Two]
  type Four = Plus[Two, Two]
  type Five = Two + Three
  
  transparent inline def toIntT[N <: Nat]: Int =
    inline scala.compiletime.erasedValue[N] match
      case _: Zero => 0
      case _: Succ[n] => toIntT[n] + 1

def test() =
  import Nat._
  summon[Plus[Zero, Zero] =:= Zero]
  summon[Succ[Succ[Zero]] =:= Two ]
  summon[Succ[Succ[Succ[Succ[Succ[Zero]]]]] =:= Five]
  summon[One =:= Inc[Zero]]
  summon[Dec[One] =:= Zero]
  summon[Dec[Two] =:= One]
  

  // summon[Succ[Succ[Succ[Succ[Succ[Zero]]]]] =:= Four ]


