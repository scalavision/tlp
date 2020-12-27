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

  inline def run[T](inline q: Quoted[Query[T]]) = ${ runImpl('q )}
  def runImpl[T : Type](q: Expr[Quoted[Query[T]]])(using ctx: Quotes): Expr[String] = 
    import ctx.reflect._

    lazy val unlift = new Unlifter()
    import unlift._

    lazy val unseal = new UnsealUtil()
    import unseal._

    object QuotedBlockPuller:
      def astParse(expr: Expr[Any]): Ast = expr match

        case '{($q: Quoted[tt]).unquote } =>
          astParse(q)

        case '{Quoted.apply[tt]($ast) } =>
          unlift(ast)

        /*  
        case '{ Dsl.query[tt] } =>
          // looks like this in the old api I think:
          // tt.unseal.tpe.classSymbol.get.name
          val typeName = quotes.reflect.TypeRepr.of[tt].classSymbol.get.name
          Entity(typeName, List())

        case '{ ($query: Query[qt]).filter(${Lambda1(ident, body)}) } =>
          val queryAst = astParse(query)
          val identAst: ast.Ident = ast.Ident(ident)
          val bodyAst = astParse(body.asTerm.underlyingArgument.asExprOf[Any])
          pprint.pprintln(body.show)
          Filter(queryAst, identAst, bodyAst)
        // case Unseal(Select(qctx.reflect.Ident(id: String), prop)) =>
        

        //case Unseal(Apply(Select(Select(Ident(id), name), op), List(args))) =>
        case Unseal(Select(Ident(id: String), prop)) =>
          Property(ast.Ident(id), prop)
          //report.throwError("STOP!")
        */

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
    print("AST tree: ")
    pprint.pprintln(ast)

    '{ "foo" }

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
          pprint.pprintln(body.show)
          Filter(queryAst, identAst, bodyAst)
        // case Unseal(Select(qctx.reflect.Ident(id: String), prop)) =>

        //case Unseal(Apply(Select(Select(Ident(id), name), op), List(args))) =>
        case Unseal(Select(Ident(id: String), prop)) =>
          Property(ast.Ident(id), prop)
          //report.throwError("STOP!")

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

          
    // Lifts the value back into an Expr
    object Lifter:
      def apply(astValue: ast.Ast): Expr[ast.Ast] = astValue match

        case Entity(name, List()) =>
          val nameExpr = Expr(name)
          // we splice the nameExpr into the Entity expr.
          '{ Entity($nameExpr, List()) }

        case Filter(query, alias, body) =>
          '{ Filter(${Lifter(query)}, ${Lifter.apply(alias).asInstanceOf[Expr[ast.Ident]]}, ${Lifter(body)}) }

        case ast.Ident(id: String) =>
          '{ ast.Ident(${Expr(id)}) }

        case Property(id, name) => '{ Property(${Lifter(id)}, ${Expr(name)}) }

        case _ =>
          report.throwError(s"""|
          can not lift the following AST:
          ${pprint.pprintln(astValue)}
          |""".stripMargin
        )

    val quillAst: Ast = Parser.astParse(quotedRaw)
    val liftedAst = Lifter.apply(quillAst)

    '{ Quoted($liftedAst) }




