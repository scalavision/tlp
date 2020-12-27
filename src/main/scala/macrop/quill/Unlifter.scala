package macrop.quill

import io.getquill.ast
import ast._

import scala.quoted._

class Unlifter(using qctx: Quotes):
  import qctx.reflect._
  
  val unsealer = new UnsealUtil()
  import unsealer._

  def apply(expr: Expr[Ast]): Ast = expr match
    case '{Entity(${Unseal(Literal(StringConstant(name)))}: String, $list) } =>
      Entity(name, List())

    case '{Filter($queryAst, $idAst, $propertyAst) } => 
      val query: Ast = apply(queryAst)
      val id: ast.Ident = apply(idAst).asInstanceOf[ast.Ident]
      val prop: Ast = apply(propertyAst)
      Filter(query, id, prop)

    case '{ ast.Ident(${Unseal(Literal(StringConstant(name)))} ) } =>
      ast.Ident(name)

    case '{ Property($insideAst, ${Unseal(Literal(StringConstant(name)))})} =>
      val inside = apply(insideAst)
      ast.Property(inside, name)

    case _ => report.throwError(s"unable to Unlift ${pprint.apply(expr)}")