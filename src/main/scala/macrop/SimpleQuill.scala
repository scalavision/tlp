package macrop

import scala.quoted._

object SimpleQuill:

  /**
   * Lifting: Ast => Expr[Ast]
   * Unlifting: Expr[Ast] => Ast (similar to parsing, but easier, because you know you have a ligit Ast tree)
   * Parsing: T <: Query[T] => Expr[Ast]
   */

  /**
   * In quill the following functions are only used for parsing, i.e.
   * to build up the query. This is a trait in real life.
   */
  class Query[T]:
    def filter(e: T => Boolean): Query[T] = throw new IllegalArgumentException("can not synthesize during runtime")
    def map[R](e: T => R): Query[R] = throw new IllegalArgumentException("can not synthesize during runtime")
    def flatMap[R](e: T => Query[R]): Query[R] = throw new IllegalArgumentException("can not synthesize during runtime")

  /**
   * This is an extension to query, to isolate the `insert` statement from
   * the others.
   */
  /*
  trait EntityQuery:
    def insert[T](newRow: T): T
  */
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
      pprint.pprintln(expr.show)
      Some(expr.asTerm)
  
  inline def quote[T](inline code: T): Quoted[T] = ${ quoteImpl('code) }
  def quoteImpl[T](code: Expr[T])(using ctx: Quotes, t: Type[T]): Expr[Quoted[T]] =
    import quotes.reflect._
    val parsedClassName = code match
      // here we should actually returned Entity(name), but that adds more complexity
      // that will be explained later
      case '{EntityQuery.apply( ${Unseal(Literal(StringConstant(name))) } ) } => name

    '{ Quoted(Entity(${Expr(parsedClassName)})) }