package macrop

import scala.quoted._

object SimpleMacro:

  inline def printTheTree(inline tree: Any): Any = ${ printTheTreeImpl('tree) }
  def printTheTreeImpl(tree: Expr[Any])(using qctx: Quotes): Expr[Any] =
    import qctx.reflect._
    println("inside the tree!")
    pprint.pprintln(tree.asTerm.underlyingArgument)
    pprint.pprintln(tree.show)
    println("leaving the tree!")
    tree
  
  
  inline def getMyTree(inline tree: String): String = ${ getMyTreeImpl('tree) }
  def getMyTreeImpl(tree: Expr[String])(using qtx: Quotes): Expr[String] =
    import qtx.reflect._

    pprint.pprintln(tree.show)

    println("underlying:")
    pprint.pprintln(tree.asTerm.underlyingArgument)

    println("tree structure:")
    Printer.TreeCode.show(tree.asTerm.underlyingArgument)

    val output = tree.asTerm.underlyingArgument match
      case Apply(Select(Literal(StringConstant(text)), "toUpperCase"), List()) =>
        println(s"we are uppercasing: ${text}")
        Literal(StringConstant("Yay"))
      case x => 
        pprint.pprintln(x)
        println("no uppercasing")
        Literal(StringConstant("Nay"))

    val outExpr = output.asExprOf[String]

    '{ ${outExpr}.toUpperCase() }

  // returning the quoted expression directly
  inline def getMyTree2(inline tree: String): String = ${ getMyTreeImpl2('tree) }
  def getMyTreeImpl2(tree: Expr[String])(using qtx: Quotes): Expr[String] =
    import qtx.reflect._

    pprint.pprintln(tree.show)

    println("underlying:")
    pprint.pprintln(tree.asTerm.underlyingArgument)

    println("tree structure:")
    Printer.TreeCode.show(tree.asTerm.underlyingArgument)

    val output = tree.asTerm.underlyingArgument match
      case Apply(Select(Literal(StringConstant(text)), "toUpperCase"), List()) =>
        println(s"we are uppercasing: ${text}")
        '{ ${Expr(text)}.substring(1, 3) }
      case x => 
        '{ "Nay"}

    val outExpr = output.asExprOf[String]

    '{ ${outExpr}.toUpperCase() }
  


 /**
 * Dotty Macro ASTs consists of two universes
 * 
 * Expr                             |    Tree / Term
 *                                  |
 * Typed                            |  This side can be invalidly typed.
 *                                  |
 * '{ ($foo: Foo).bar }             |  Apply(Select(Ident("foo"), "bar"), List())
 * 
 * Unseal, i.e. Expr[T].asTerm --->  Expr[T] => Tree
 * 
 *                  Expr[Any]      <---  Seal
 *                  Expr[T]        <---  Seal.cast[T], i.e. some term .asExprOf[T]
 */
  
  inline def getMyTree3(inline tree: String): String = ${ getMyTreeImpl3('tree) }
  def getMyTreeImpl3(tree: Expr[String])(using qtx: Quotes): Expr[String] =
    import quotes.reflect._

    pprint.pprintln(tree.show)
    
    println("underlying:")
    pprint.pprintln(tree.asTerm.underlyingArgument)

    println("tree structure:")
    Printer.TreeCode.show(tree.asTerm.underlyingArgument)

    val inTheLandOfTrees: Term = tree.asTerm.underlyingArgument match
      case Literal(StringConstant(text)) => Literal(StringConstant(text))
      case _ => Literal(StringConstant("Nay"))


    val makeExpr = inTheLandOfTrees.asExprOf[String]

    val output = tree match
      //case '{ ($(UnsealAuto(Literal(ConstantString(ourString)))): String).toUpperCase  } =>
      // we can now match against the expression of string directly, including
      // the function call ...
      case '{ ( ${ourString}: String ).toUpperCase  } =>
        println(s"we are uppercasing: ${ourString}")

        '{ ${ourString}.substring(1, 3) } 
      case x => 
        '{ "Nay"}

    val outExpr = output.asExprOf[String]

    '{ ${outExpr}.toUpperCase() }

  // We can also use a hybrid approach, where we mix and match on
  // both Expr and Term

  object UnsealAuto:
    def unapply(expr: Expr[String])(using ctx: Quotes): Some[ctx.reflect.Term] = 
      import ctx.reflect._
      Some(expr.asTerm)
  
  inline def getMyTree4(inline tree: String): String = ${ getMyTreeImpl4('tree) }
  def getMyTreeImpl4(tree: Expr[String])(using qtx: Quotes): Expr[String] =
    import quotes.reflect._

    pprint.pprintln(tree.show)
    
    println("underlying:")
    pprint.pprintln(tree.asTerm.underlyingArgument)

    println("tree structure:")
    Printer.TreeCode.show(tree.asTerm.underlyingArgument)

    val output = tree match
      // The `tree` expression gets sent into unapply, and it
      // returns the Term, where we extract out the String `str` directly from this.
      case '{ ( ${ UnsealAuto(Literal(StringConstant(str)))}: String ).toUpperCase  } =>
        // str is now an actual string
        println(s"we are uppercasing: ${str}")
        // bring it back as an expression !
        Literal(StringConstant(str)).asExprOf[String]
        // '{ ${  } }
        // ${ str.substring(1, 3) }
        // '{ "Yay" }

      case x => 
        '{ "Nay"}

    val outExpr = output.asExprOf[String]

    '{ ${outExpr}.toUpperCase() }
  