package macrop.quill

import scala.quoted._

object ParseMac:
  inline def apply(inline anyRaw: Any): Unit = ${ parseImpl('anyRaw )}
  def parseImpl(anyRaw: Expr[Any])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect._
    lazy val unsealUtil = new UnsealUtil()
    import unsealUtil._
    val any = anyRaw.asTerm.underlyingArgument.asExprOf[Any]

    any match
      case Unseal(Apply(Select(id1, "=="), List(b))) =>
          println(s"================== MATCH generic filter expression =====================")
          pprint.pprintln(id1)
          pprint.pprintln(b)
          
      case Unseal(Apply(Select(Ident(a), "=="), List(Ident(b)))) =>
        println(s"================== MATCH Ident =====================")
        pprint.pprintln(s"${any.show}")
        println(s"==============================================")
      case Unseal(Apply(Select(Literal((nr)), "=="), List(Ident("b"))))  =>
        println(s"================== MATCH Literal =====================")
        pprint.pprintln(s"${any.show}")
        println(s"==============================================")
      case _ =>
        println(s"~~~~~~~~~~~~~~~ NO MATCH !!! ~~~~~~~~~~~~~~~~")
        pprint.pprintln(s"${any.show}")
        PrintMac(any)

    '{ () }