package macroHList

import scala.language.existentials
import scala.language.higherKinds

import TypeOperators._

object Poly {
  
  trait Poly1[Arg1[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x: Arg1[T]): (R forSome {type R})
  }

  trait Poly2[Arg1[_], Arg2[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x1: Arg1[T], x2: Arg2[T]): (R forSome {type R})
  }

  type ForAll[T] = DummyImplicit

}
