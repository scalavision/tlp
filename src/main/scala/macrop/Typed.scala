package macrop

import scala.quoted._
//import runtime.Expr

inline def classNameOf[T]: String = ${ classNameOfImpl[T] }

def classNameOfImpl[T: Type](using qctx: Quotes) =
  import quotes.reflect._
  Expr(TypeTree.of[T].symbol.name)
