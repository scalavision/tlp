package refactoring.vector

object ValueLevel:

  sealed trait Vec:
    def size: Int
    def ::(head: Int): Vec = NonEmptyVec(head, this)
    def +(that: Vec): Vec
    def ++(that: Vec): Vec

  case object VNil extends Vec:

    def size: Int = 0

    def + (that: Vec) =
      require(that == VNil)
      this

    def ++(that: Vec): Vec = that
  
  case class NonEmptyVec(head: Int, tail: Vec) extends Vec:
    def size: Int = 1 + tail.size
    def ++(that: Vec): Vec = NonEmptyVec(head, tail ++ that)
    def +(that: Vec): Vec = 
      require(this.size == that.size)
      that match
        case NonEmptyVec(h, t) => (head + h) :: (that + t)
        case VNil => throw new Exception("Boom!! Can only add vectors of equal size")

  def test() =
    val sum = (1 :: 2 :: VNil) + (3 :: 4 :: VNil)
    assert(sum == 4 :: 6 :: VNil)

import refactoring.peano._





sealed trait Vec[Size <: Nat]:
  self =>
  def size: Int

  def ::(head: Int): NonEmptyVec[Size] =
    NonEmptyVec[Size](head, self)

  def +(that: Vec[Size]): Vec[Size]

  def ++[TailSize <: Nat](that: Vec[TailSize]): Vec[Plus[Size, TailSize]]

case object VNil extends Vec[Zero]:
  self =>
  import Nat._
  def size: Int = scala.Tuple.Size[Zero]
  def + (that: Vec[Zero]): Vec[Zero] = self
  def ++[Size <: Nat](that: Vec[Size]) = that

case class NonEmptyVec[TailSize <: Nat](head: Int, tail: Vec[TailSize]) extends Vec[Succ[TailSize]]:
  import Nat._
  type Size = Succ[TailSize]
  def size: Int = 1 + tail.size

  def ++[ThatSize <: Nat](that: Vec[ThatSize]) = 
    NonEmptyVec[TailSize + ThatSize](head, tail ++ that)

  def +(that: Vec[Size]) = 
    that match
      case NonEmptyVec(h, t) => (head + h) :: (tail + t)

def test() =
  val v1 = 1 :: 2 :: VNil
  val v2 = 2 :: 3 :: VNil
  val vx = 1 :: VNil

  // val failure = v1 + vx
  
  val v3 = (v1 + v2) ++ (v1 + v2)
  println(v3)