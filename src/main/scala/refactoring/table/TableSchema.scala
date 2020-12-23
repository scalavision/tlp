package refactoring.table

enum HList:
  case HNil()
  case HCons[H, T <: HList](h: H, t: T)

import HList._

type Concat[Xs <: HList, Ys <: HList] <: HList = Xs match
  case HNil => Ys
  case HCons[h, t] => HCons[h, Concat[t, Ys]]

/**
 * We search for the E in the Xs, and if it is found
 * we return only the tail, otherwise, we keep searching.
 */
type Remove[E, Xs <: HList] <: HList = Xs match
  // E is matched, t is binding
  case HCons[E, t] => t
  case HCons[h, t] => HCons[h, Remove[E, t]]

type Join[K, S1 <: HList, S2 <: HList] =
  HCons[K, Concat[Remove[K, S1], Remove[K, S2]]]

def join[S1 <: HList, S2 <: HList, K](t1: Table[S1], t2: Table[S2], key: K): Table[Join[K, S1, S2]] = ???

case class Table[Schema <: HList]()
type X = HCons["age", HCons["name", HNil]]
type Y = HCons["name", HCons["unit", HNil]]
val xs: Table[X] = ???
val ys: Table[Y] = ???
val zs: Table[HCons["name", HCons["age", HCons["unit", HNil]]]] = join(xs, ys, "name")


def concat[Xs <: HList, Ys <: HList](xs: Xs, ys: Ys): Concat[Xs, Ys] =
  xs match
    case xs: HNil => ys
    case xs: HCons[h, t] => HCons(xs.h, concat(xs.t, ys))

val cols = HCons("age", HCons("name", HNil()))
val zz = concat(cols, cols)

