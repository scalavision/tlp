package refactoring.unsealedtraits

trait Foo
trait Bar

/**
 * Semantics of match types corresponds to the
 * semantics of expressions, so that cases are
 * evaluated in order. In example below, we can not
 * disambiguate Bar from Foo. We will get stuck on
 * Foo, hence Bar will not be evaluated and we have
 * a compile time error.
 */

type M[T] = T match
  case Foo => String
  case Bar => Int

def m[T](t: T): M[T] = t match
  case _: Foo => ""
  case _: Bar => 1

def test() =
  summon[M[Foo] =:= String]
  // will not work
  // summon[M[Bar] =:= Int]

  val str = m[Foo](new Foo{})
  val bar = m[Bar](new Bar{})

  // crash and burn ..
  val x = m[String]("")