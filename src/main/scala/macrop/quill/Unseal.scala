package macrop.quill

import scala.quoted._

class UnsealUtil(using qctx: Quotes):

  object Unseal:
    def unapply(expr: Expr[_])(using ctx: Quotes): Option[ctx.reflect.Term] = 
      import ctx.reflect._
      Some(expr.asTerm.underlyingArgument)
  
    