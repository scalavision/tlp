package macrop.quill

import io.getquill.ast
import io.getquill.ast._

import scala.quoted._

case class Quoted[T](ast: Ast):
  def unquote = throw new IllegalArgumentException("Only a compile-time-construct")

class Query[T]:
  def filter(e: T => Boolean): Query[T] = throw new IllegalArgumentException("This can only be used inside quoted block")
  def map[R](e: T => R): Query[R] = throw new IllegalArgumentException("This can only be used inside quoted block")
  def flatMap[R](e: T => Query[R]): Query[R] = throw new IllegalArgumentException("This can only be used inside quoted block")

object Dsl:

  def query[T]: Query[T] = throw new IllegalArgumentException("This can only be used inside quoted block")

  inline def run[T](inline qRaw: Quoted[Query[T]]) = ${ runImpl('qRaw )}
  def runImpl[T : Type](qRaw: Expr[Quoted[Query[T]]])(using ctx: Quotes): Expr[String] = 
    import ctx.reflect._

    lazy val unlift = new Unlifter()
    import unlift._

    lazy val unseal = new UnsealUtil()
    import unseal._

    // simplifying the AST expression by removing unneeded wrapping
    // of scala AST components, especially the Inlined wrapper.
    val q = qRaw.asTerm.underlyingArgument.asExprOf[Any]

    object QuotedBlockPuller:
      def astParse(expr: Expr[Any]): Ast = expr match

        case '{($q: Quoted[tt]).unquote } =>
          astParse(q)

        case '{Quoted.apply[tt]($ast) } =>
          unlift(ast)
      
        case Unseal(Typed(inside /*Term*/, _)) =>
          astParse(inside.asExprOf[Any])

        case _ =>
          report.throwError(
            s"""|
            |syntax error, the following code (AST expression)
            | ${expr.show} is unsupported, please read the manual!
            |
            |${pprint.pprintln(expr.asTerm.underlyingArgument)}
            |""".stripMargin
          )      

        
    val ast = QuotedBlockPuller.astParse(q)



    println(
      s"""|======================== Query AST =========================
      |${pprint.pprintln(ast)}
      |==============================================================
      |""".stripMargin
    )

    val dialect = new io.getquill.PostgresDialect{}
    val (_, stmt) = dialect.translate(ast)(io.getquill.Literal)
    val sql = stmt.toString

    //'{ ${Expr(sql)} }
    Expr(sql)

  inline def unquote[T](inline quoted: Quoted[T]): T = ${ unquoteImpl[T]('quoted) }
  def unquoteImpl[T : Type](quoted: Expr[Quoted[T]])(using Quotes): Expr[T] = '{ $quoted.unquote }

  inline def quote[T](inline qt: T): Quoted[T] = ${ quoteImpl[T]('qt)}
  def quoteImpl[T](qt: Expr[T])(using qctx: Quotes, t: Type[T]): Expr[Quoted[T]] = 
    import qctx.reflect.{TypeRepr => TType, _}

    lazy val unsealer = new UnsealUtil()
    import unsealer._

    lazy val unlift = new Unlifter()

    val quotedRaw = qt.asTerm.underlyingArgument.asExprOf[Any]


    // helper parsers taken from TastyMatcher.scala from dotty_test code
    // used to be able to parse lambda expressions
    object TypedMatroshkaTerm {
      def recurse(innerTerm: Term): Term = innerTerm match {
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other
      }

      def unapply(term: Term): Option[Term] = term match {
        case Typed(tree, _) => Some(recurse(tree))
        case other => None
      }
    }

    object TypedMatroshka {
      def unapply(term: Expr[Any]): Option[Expr[Any]] = 
        TypedMatroshkaTerm.unapply(term.asTerm).map(_.asExpr)
    }

    object UntypeExpr {
      def unapply(expr: Expr[_]): Option[Expr[_]] = 
        Untype.unapply(expr.asTerm).map(_.asExpr)

      def apply(expr: Expr[_]): Expr[_] = Untype.unapply(expr.asTerm).map(_.asExpr).get
    }

    // Always match (whether ast starts with Typed or not). If it does, strip the Typed node.
    object Untype {
      def unapply(term: Term): Option[Term] = term match {
        case TypedMatroshkaTerm(t) => Some(t)
        case other => Some(other)
      }

      def apply(term: Term) = Untype.unapply(term).get
    }

    object Lambda1 {
      def unapply(expr: Expr[_]): Option[(String, quoted.Expr[_])] =
        unapplyTerm(expr.asTerm).map((str, expr) => (str, expr.asExpr))

      def unapplyTerm(term: Term): Option[(String, Term)] = Untype(term) match {
        case Lambda(List(ValDef(ident, _, _)), methodBody) => Some((ident, methodBody))
        case Block(List(), expr) => unapplyTerm(expr)
        case _ => None
      }
    }

    // Parses the Expr into a value
    object Parser:
      def astParse(expr: Expr[Any]): Ast = expr match

        // matching on Unseal directly, we are doing the p => p == "joe" part
        case Unseal(Apply(Select(a, "=="), List(b))) =>
          val aAst = astParse(a.asExprOf[Any])
          val bAst = astParse(b.asExprOf[Any])
          BinaryOperation(aAst, EqualityOperator.`==`, bAst)

        case Unseal(Apply(Select(a, "!="), List(b))) =>
          val aAst = astParse(a.asExprOf[Any])
          val bAst = astParse(b.asExprOf[Any])
          BinaryOperation(aAst, EqualityOperator.`!=`, bAst)

        // Would need to add for all literal types of Scala?
        // look at scala.quoted.matchers.Const
        case Unseal(Literal(StringConstant(value))) =>
          ast.Ident(value)

        case '{($q: Quoted[tt]).unquote } =>
          astParse(q)

        case '{Quoted.apply[tt]($ast) } =>
          unlift(ast)

        case '{ Dsl.query[tt] } =>
          // looks like this in the old api I think:
          // tt.unseal.tpe.classSymbol.get.name
          val typeName = quotes.reflect.TypeRepr.of[tt].classSymbol.get.name
          Entity(typeName, List())

        case '{ ($query: Query[qt]).filter(${Lambda1(ident, body)}) } =>
          val queryAst = astParse(query)
          val identAst: ast.Ident = ast.Ident(ident)
          val bodyAst = astParse(body.asTerm.underlyingArgument.asExprOf[Any])
          Filter(queryAst, identAst, bodyAst)
        
        case Unseal(Select(Ident(id: String), prop)) =>
          Property(ast.Ident(id), prop)

        case Unseal(Typed(inside /*Term*/, _)) =>
          astParse(inside.asExprOf[Any])

        case _ =>
          report.throwError(
            s"""|
            |
            |syntax error, the following code (AST expression)
            |is unsupported:
            |
            |==================== FULL =====================
            |${expr.asTerm.underlyingArgument}
            |
            |==================== SIMPLE =====================
            |${expr.show}
            |""".stripMargin
          )

          
    trait Lifter[T]:
      def lift(element: T): Expr[T]
    
    // Lifts the value back into an Expr
    object Lifter:


      given Lifter[ast.Ident] = new Lifter[ast.Ident]:
        def lift(element: ast.Ident): Expr[ast.Ident] = element match
          case ast.Ident(id: String) =>
            '{ ast.Ident(${Expr(id)}) }
        


      import BooleanOperator._
      import NumericOperator._
      import StringOperator.`startsWith`
      import SetOperator.`contains`

      given Lifter[BinaryOperator] = new Lifter[BinaryOperator]:
        def lift(op: BinaryOperator): Expr[BinaryOperator] = op match
          case _ if (op == EqualityOperator.`==` ) => '{ EqualityOperator.`==` }
          case _ if (op == EqualityOperator.`!=` ) => '{ EqualityOperator.`!=` }
          //case _: ne.type => '{ EqualityOperator.`!=` }
          case NumericOperator.+ => '{ NumericOperator.+ }
          case NumericOperator.- => '{ NumericOperator.- }
          case NumericOperator.* => '{ NumericOperator.* }
          case NumericOperator./ => '{ NumericOperator./ }
          case NumericOperator.% => '{ NumericOperator.% }
          case NumericOperator.> => '{ NumericOperator.> }
          case NumericOperator.< => '{ NumericOperator.< }
          case StringOperator.+ => '{ StringOperator.+ }
          case BooleanOperator.|| => '{ BooleanOperator.|| }
          case BooleanOperator.&& => '{ BooleanOperator.&& }

      given Lifter[String] = new Lifter[String]:
        def lift(str: String): Expr[String] = Expr(str)
      
      /*
      implicit def liftLifter[T: Type](implicit elementLifter: Lifter[T]): Lifter[List[T]] = new Lifter[List[T]]:
        def lift(list: List[T]): Expr[List[T]] =
          val listOfLifts = list.map(e => elementLifter.lift(e))
          Expr.ofList(listOfLifts)
      */
      // This is the way to do implicit def it in Scala 3
      given listLifter[T: Type](using elementLifter: Lifter[T]): Lifter[List[T]] with
        def lift(list: List[T]): Expr[List[T]] =
          val listOfLifts = list.map(e => elementLifter.lift(e))
          Expr.ofList(listOfLifts)
    
      given Lifter[PropertyAlias] = new Lifter[PropertyAlias]:
        def lift(alias: PropertyAlias): Expr[PropertyAlias] =
          alias match
            case PropertyAlias(paths, alias) =>
              // List already has a function lift, and we are not able to override it
              // therefor we call this implicit def directly
              //'{ PropertyAlias(${liftLifter[String].lift(paths)}, ${alias.lift})}
              // oki, we just add another method, liftAlias that we can use for List
              '{ PropertyAlias(${paths.liftAlias}, ${alias.lift})}

      given Lifter[ast.Ast] = new Lifter[ast.Ast]:
        def lift(astValue: ast.Ast): Expr[Ast] = astValue match

          case id: ast.Ident => id.lift

          case Entity(name, propertyAlias) =>
            val nameExpr = Expr(name)
            // we splice the nameExpr into the Entity expr.
            '{ Entity($nameExpr, ${propertyAlias.liftAlias}) }

          case Filter(query, alias, body) =>
            '{ Filter(${Lifter(query)}, ${alias.lift}, ${Lifter(body)}) }

          case Property(inner, name) => '{ Property(${inner.lift}, ${name.lift}) }

          case BinaryOperation(a, binaryOp, b) => 
            '{ BinaryOperation(${a.lift}, ${binaryOp.lift}, ${b.lift}) }

          case ast.Constant(value: String) =>
            '{ ast.Constant(${Expr(value)}) }
          
          case _ =>
            report.throwError(s"""|
            can not lift the following AST:
            ${pprint.pprintln(astValue)}
            |""".stripMargin
          )

      extension [T](element: T)(using lifter: Lifter[T])
        def lift = lifter.lift(element)
        def liftAlias = lifter.lift(element)

      def apply(astValue: ast.Ast): Expr[ast.Ast] = astValue.lift

    val quillAst: Ast = Parser.astParse(quotedRaw)
    val liftedAst = Lifter.apply(quillAst)

    '{ Quoted($liftedAst) }




