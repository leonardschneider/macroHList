package macroHList

import scala.language.existentials
import scala.language.higherKinds

import TypeOperators._

object Poly {
 
  type ForAll[T] = DummyImplicit

  trait Poly1[Arg1[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x: Arg1[T]): (R forSome {type R})
  }
  object Poly1 {
    def apply[T1, R](f: T1 => R) = new Poly1[Const[T1]#Kind, ForAll] {
      def apply[T: ForAll](x: T1) = f(x)
    }
    def const[U](y: U) = new Poly1[Id, ForAll] {
      def apply[T: ForAll](x: T) = y
    } 
  }

  trait Poly2[Arg1[_], Arg2[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x1: Arg1[T], x2: Arg2[T]): (R forSome {type R})
  }



}
