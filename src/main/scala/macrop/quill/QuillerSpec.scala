package macrop.quill

import Dsl._

case class Person(name: String, age: Int)
case class Person3(name: String, isHuman: Boolean, isAlien: Boolean)
case class Person2(name: Name, age: Int)
case class Name(first: String, last: String)

/**
 * 1. Create a function taking in an object, i.e. query[Person]
 * 2. Parse query[Person] into an abstract syntax tree, i.e. quote { .. }
 * 3. Transform the quoted expression into sql, or whatever your target language is
 */
object QuillerSpec:

  implicit inline def autoUnquote[T](inline quoted: Quoted[T]): T = unquote(quoted)

  // given Conversion[T]
  def test() =
    // Quoted[Query[Person]](Entity("Person"))

    inline def q = quote { query[Person3] }
    inline def q1 = quote { q.filter(p => p.isHuman) }
    pprint.pprintln(q1)
    println(q)
    run(q)
    //println(result)
    //PrintMac(q)
    //PrintMac(4: Long)

  def test2() = ()
    /*
    def p = Person2(Name("Joe", "Blogger"), 44)
    println("test2")
    PrintMac(p.name.first)
    */