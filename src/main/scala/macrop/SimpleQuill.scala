package macrop

import scala.quoted._

object SimpleQuill:

  enum Ast:
    case Entity(value: String)
  
  import Ast._
  case class EntityQuery(value: String)

  inline def query[T]: EntityQuery = ${ queryImpl[T] }
  def queryImpl[T](using ctx: Quotes, t: Type[T]): Expr[EntityQuery] =
    import quotes.reflect._
    val className: String = TypeTree.of[T].symbol.name 
    // className needs to be converted back into an Expr
    // if not we end up with the error:
    //   access to value at wrong staging level
    //     - the definition is at level 0
    //     - but the access is ast level 1
    '{ EntityQuery.apply(${Expr(className)}) } 
    
  case class Quoted[T](ast: Ast)

  object Unseal:
    def unapply(expr: Expr[String])(using ctx: Quotes): Some[ctx.reflect.Term] = 
      import ctx.reflect._
      Some(expr.asTerm)
  
  inline def quote[T](inline code: T): Quoted[T] = ${ quoteImpl('code) }
  def quoteImpl[T](code: Expr[T])(using ctx: Quotes, t: Type[T]): Expr[Quoted[T]] =
    import quotes.reflect._
    val parsedClassName = code match
      // here we should actually returned Entity(name), but that adds more complexity
      // that will be explained later
      case '{EntityQuery.apply( ${Unseal(Literal(StringConstant(name))) } ) } => name

    '{ Quoted(Entity(${Expr(parsedClassName)})) }