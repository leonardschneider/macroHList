package macroHList

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.macros.Context

trait ImplicitTupleContext extends HListContext {
  def mkTuple(tup: c.Expr[T forSome {type T}]) =
    fromTuple(AbsExpr(tup)).toExpr
}

object Implicits {

  implicit def tupleToHList(tup: T forSome {type T}): R forSome {type R <: HList} =
    macro ImplicitTupleContext.mkTuple

}

