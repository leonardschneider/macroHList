package macroHList

import scala.language.existentials
import scala.language.higherKinds

import TypeOperators._

object Poly {
 
  type ForAll[T] = DummyImplicit

  type Compose[A[_], B[_]] = { type Kind[X] = A[B[X]] }

  trait Poly1[Arg1[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x: Arg1[T]): (R forSome {type R})
    /*def compose[GArg1[_], G <: Poly1[GArg1, ArgImpl] {
        def apply[T: ArgImpl](x: GArg1[T]): (R forSome {type R <: Arg1[_]})
      }](g: G) = {
      val f = this
      new Poly1[GArg1, ArgImpl] {
        def apply[T: ArgImpl](x: GArg1[T]) = f(g(x))(implicitly[ArgImpl[T]])
      }
    }*/
  }
  object Poly1 {
    def apply[T1, R](f: T1 => R) = new Poly1[Const[T1]#Kind, ForAll] {
      def apply[T: ForAll](x: T1) = f(x)
    }
    def const[U](y: U) = new Poly1[Id, ForAll] {
      def apply[T: ForAll](x: T) = y
    }
  }

  // utilities for Poly1
  val identity = new Poly1[Id, ForAll] {
    def apply[T: ForAll](x: T) = x
  }


  trait Poly2[Arg1[_], Arg2[_], ArgImpl[_]] {
    def apply[T: ArgImpl](x1: Arg1[T], x2: Arg2[T]): (R forSome {type R})
  }



}
