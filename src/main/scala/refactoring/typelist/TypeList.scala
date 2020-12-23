package refactoring.typelist

import refactoring.peano._

object Scala2:
  sealed trait TypeList:
    type size <: Nat
  sealed trait TNil extends TypeList:
    type size = Zero
  sealed trait ::[H <: Nat, T <: TypeList] extends TypeList
    // not legal in Scala 3
    //type size = Succ[T#size]


sealed trait TypeList [Size <: Nat]
sealed trait TNil extends TypeList [Zero]
sealed trait ::[H <: Nat, TailSize <: Nat, T <: TypeList[TailSize]] extends TypeList [Succ[TailSize]]

object TypeList
  // type ::[N1 <: Nat, N2 <: Nat, T1 <: TypeList[N1], T2 <: TypeList[N2]] = ???


def test = ???
  
  // type L = [Two, One, One] :: TNil

/*
sealed trait ::[H <: Nat, T <: TypeList] extends TypeList[Succ[]]
  type Size = Plus[] */