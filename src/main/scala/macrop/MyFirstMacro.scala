package macrop

import scala.quoted._
import runtime.Expr
//import io.getquill.util.Messages._

object MyFirstMacro:
  // ' - quote
  // $ - splice
  // asTerm: Term is the same as a Tree
  // mac = macro
  //
  inline def myMac(foo: String) =
    ${ myMacImpl('foo ) }

  def myMacImpl(foo: Expr[String])(using ctx: Quotes): Expr[String] =
    import quotes.reflect._

    println("############## DEBUG AT COMPILETIME #################")
    println(foo.show)
    println(foo.value)
    println(foo.isExprOf[String])
    pprint.pprintln(foo.asTerm) // Inlined(EmptyTree, List(), Literal(Constant(hello ..))) --> called showExtractors in earlier version of scala 3?
    
    println("tpe:")
    println(foo.asTerm.tpe) // ConstantType(Constant(hello ..))

    println("############## END #################")
    foo

  inline def myMac2(inline foo: String) =
    ${ myMacImpl2('foo ) }

  def myMacImpl2(foo: Expr[String])(using ctx: Quotes): Expr[String] =
    import quotes.reflect._

    println("############## DEBUG AT COMPILETIME #################")
    println(foo.show)
    
    println("isString:")
    println(foo.isExprOf[String])

    println("asTerm:")
    pprint.pprintln(foo.asTerm) // Inlined(EmptyTree, List(), Literal(Constant(hello ..))) --> called showExtractors in earlier version of scala 3?
    
    println("filter out intermediate expressions:")
    pprint.pprintln(foo.asTerm.underlyingArgument)

    println("filter out intermediate expressions, with extractors:")
    pprint.pprintln(foo.asTerm.underlying)

    println("\ntpe:")
    println(foo.asTerm.tpe) // ConstantType(Constant(hello ..))
    println(foo.asTerm.show) // ("some string value": java.lang.String)

    println("\nvalue:")
    pprint.pprintln(foo.value)


    def parseFunction(expr: Term): Option[String] = expr match
      case Typed(value, tpe) => parseFunction(value)
      case Literal(StringConstant(value)) => Some(value)
      case other => None

    println("parsed function:")  

    parseFunction(foo.asTerm.underlyingArgument) match
      case Some(value) => println(s"The expression ${foo.show} is a compile-time string")
      case None => println(s"the expression ${foo.show} is not a compile-time string")

    println("############## END #################")
    foo
