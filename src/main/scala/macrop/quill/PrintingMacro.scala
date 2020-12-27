package macrop.quill

import scala.quoted._

object PrintMac:

  inline def apply(inline any: Any): Unit = ${ printImpl('any )}
  def printImpl(expr: Expr[Any])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect._
    println("================= The Short Version ==================")
    println(pprint.pprintln(expr.show))
    println("================= The Long Version ===================")
    println(pprint.pprintln(expr.asTerm.underlyingArgument))
    '{ () } // just returning an expr of unit

  