package macroHList

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.macros.Context

object Implicits {

  implicit def tupleToHList(tup: T forSome {type T}): R forSome {type R <: HList} = macro tupleToHListImpl

  def tupleToHListImpl(c: Context)(tup: c.Expr[T forSome {type T}]) = {
    import c.universe._

    import HList._
    val hl = hListContext(c)
    hl.fromTuple(hl.AbsExpr(tup)).toExpr
  }

}

