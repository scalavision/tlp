package macrop.quill

import io.getquill.ast
import ast._

import scala.quoted._

class Unlifter(using qctx: Quotes):
  import qctx.reflect._

  val unsealer = new UnsealUtil()
  import unsealer._
  import EqualityOperator.{`==` => ee, `!=` => ne }
  import BooleanOperator._
  import NumericOperator._
  import StringOperator.`startsWith`
  import SetOperator.`contains`

  def unlift(op: Expr[BinaryOperator]): BinaryOperator = 
    op match
      case '{ NumericOperator.+ } =>  NumericOperator.+
      case '{ NumericOperator.- } =>  NumericOperator.-
      case '{ NumericOperator.* } =>  NumericOperator.*
      case '{ NumericOperator./ } =>  NumericOperator./
      case '{ NumericOperator.% } =>  NumericOperator.%
      case '{ NumericOperator.> } =>  NumericOperator.>
      case '{ NumericOperator.< } =>  NumericOperator.<
      case '{ StringOperator.+ } =>  StringOperator.+
      case '{ EqualityOperator.== } =>  EqualityOperator.==
      case '{ BooleanOperator.|| } =>  BooleanOperator.||
      case '{ BooleanOperator.&& } =>  BooleanOperator.&&

  import scala.quoted.Const

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

    case Unseal(Literal(StringConstant(value))) =>
      ast.Constant(value)

    case '{ ast.Ident(${Const(name: String)})} => ast.Ident(name)
    //case '{ ast.Constant( ${ Unseal(Literal(StringConstant(value) )) } ) } =>
      // ast.Constant(value)

    case '{ ast.BinaryOperation($a, $op, $b) } =>
      ast.BinaryOperation(apply(a), unlift(op), apply(b))


    case _ => report.throwError(s"unable to Unlift ${pprint.apply(expr)}")