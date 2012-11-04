package macroHList

import scala.language.experimental.macros
import scala.reflect.macros.Context


    trait HList {
      def length: Int
      def isEmpty: Boolean
      def nonEmpty = !isEmpty
    }

    case class ::[H, T <: HList](head: H, tail: T) extends HList {
      def ::[H](h: H) = macroHList.::(h, this)
      def :+[T](t: T) = macro HList.append[T]
      def last: Any = macro HList.last
      def reverse: Any = macro HList.reverse
      def init: Any = macro HList.init
      def map[HF <: HList](hf: HF) = macro HList.map[HF]
      def length = 1 + tail.length
      def isEmpty = false
      override def toString = head.toString + " :: " + tail.toString
    }

    trait HNil extends HList {
      def ::[H](h: H) = macroHList.::(h, this)
      def :+[H](h: H) = macroHList.::(h, this)
      def length = 0
      def isEmpty = true
      override def toString = "HNil"
    }

    case object HNil extends HNil

    object HList {
      // helper functions that are often used in following macros
      def head[H: c.WeakTypeTag, T <: HList: c.WeakTypeTag](c: Context)(l: c.Tree): c.Tree =
        c.universe.reify{c.Expr[H :: T](l).splice.head}.tree
      def tail[H: c.WeakTypeTag, T <: HList: c.WeakTypeTag](c: Context)(l: c.Tree): c.Tree =
        c.universe.reify{c.Expr[H :: T](l).splice.tail}.tree

      def find[T: c.WeakTypeTag](c: Context): c.Expr[Any] = {
        import c.universe._
        sys.error("Not implemented")
      }
      def map[HF <: HList: c.WeakTypeTag](c: Context)(hf: c.Expr[HF]): c.Expr[Any] = {
        import c.universe._
        /** hf is itself a HList of functions of one argument
         *  For each element of this HList, we find the first element of hf
         *  which can be applied to this element, and apply it
         */
        /** This function returns which function of hf to apply to a given type
         */
        def getMap(hType: Type, hTree: Tree): Tree = hType match {
          case TypeRef(_, _, List(h, t)) if 
            h <:< appliedType(typeOf[Function1[_, _]], List(h, definitions.AnyTpe)) => {
              head(c)(hTree)(c.WeakTypeTag(h), c.WeakTypeTag(t))
            }
          case h if h =:= typeOf[HNil] => sys.error("Map function cannot be applied to " + hType)
          case TypeRef(_, _, List(h, t)) => {
              def genTree[H: WeakTypeTag, T <: HList: WeakTypeTag] =
                reify{c.Expr[::[H, T]](hTree).splice.tail}.tree
              getMap(hType, genTree(c.WeakTypeTag(h), c.WeakTypeTag(t)))
            }
        }
        
        sys.error("Not implemented")
      }
      def append[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[Any] = {
        import c.universe._
        /** We want to write l :+ t = (t :: l.reverse).reverse
         *  But we can't use reverse as it is in the same compilation unit
         */
        val (rev, revType) = reverseList(c)(c.prefix)
        val (hType, tType) = revType match {
          case TypeRef(_, _, List(hType, tType)) => (hType, tType)
        }
        def genTree[H: WeakTypeTag, T <: HList: WeakTypeTag] = 
          reverseList(c)(reify{t.splice :: c.Expr[::[H, T]](rev).splice})._1
        c.Expr(genTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType)))
        //sys.error("Not implemented")
      }
      def last(c: Context): c.Expr[Any] = {
        import c.universe._
        /* We want to reify something of the form tail.tail. ... .head
         * For this, we use a recursive function.
         * As a recursive function needs a fixed return type,
         * we use expression trees to opacify expression type
         */
        def gen(lType: Type, expTree: Tree): Tree = {
          //c.echo(c.enclosingPosition, "exp type: " + exp.staticType)
          lType match {
            case TypeRef(_, _, List(hType, hnil)) if hnil =:= typeOf[HNil] =>
              head(c)(expTree)(c.WeakTypeTag(hType), c.WeakTypeTag(hnil))
            case TypeRef(_, _, List(hType, tType)) =>
              gen(tType, tail(c)(expTree)(c.WeakTypeTag(hType), c.WeakTypeTag(tType)))
          }
        }
        c.Expr(gen(c.prefix.actualType.widen, c.prefix.tree))
      }
      def reverse(c: Context): c.Expr[Any] = c.Expr(reverseList(c)(c.prefix)._1)
      def reverseList[L: c.WeakTypeTag](c: Context)(l: c.Expr[L]): (c.Tree, c.Type) = {
        import c.universe._
        /** We want to use a recursive function with an accumulator
         *  which will hold the beginning of the reversed list
         */
        def gen(lType: Type, accuType: Type, expTree: Tree, accu: Tree): (Tree, Type) =
          lType match {
            case TypeRef(_, _, List(hType, hnil)) if hnil =:= typeOf[HNil] => 
              if(accuType =:= typeOf[HNil]) {
                (expTree, lType)
              }
              else {
                def genExpTree[H: WeakTypeTag, A <: ::[_, _]: WeakTypeTag] =
                  reify{c.Expr[::[H, HNil]](expTree).splice.head :: c.Expr[A](accu).splice}.tree
                (genExpTree(c.WeakTypeTag(hType), c.WeakTypeTag(accuType)),
                 appliedType(typeOf[::[_, _]].typeConstructor, List(hType, accuType)))
              }
            case TypeRef(_, _, List(hType, tType)) => {
              def genExpTree[H: WeakTypeTag, T <: HList: WeakTypeTag] =
                reify{c.Expr[::[H, T]](expTree).splice.tail}.tree
              if(accuType =:= typeOf[HNil]) {
                def genAccuTree[H: WeakTypeTag, T <: HList: WeakTypeTag] =
                  reify{c.Expr[::[H, T]](expTree).splice.head :: HNil}.tree 
                gen(tType, appliedType(typeOf[::[_, _]].typeConstructor, List(hType, accuType)),
                    genExpTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType)),
                    genAccuTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType))
                )
              }
              else {
                def genAccuTree[H: WeakTypeTag, T <: HList: WeakTypeTag, A <: ::[_, _]: WeakTypeTag] =
                  reify{c.Expr[::[H, T]](expTree).splice.head :: c.Expr[A](accu).splice}.tree
                gen(tType, appliedType(typeOf[::[_, _]].typeConstructor, List(hType, accuType)),
                  genExpTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType)),
                  genAccuTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType), c.WeakTypeTag(accuType))
                )
              }
            }
          }
        //c.echo(c.enclosingPosition, "lType: (actual) " + l.actualType + " (static) " + l.staticType)
        gen({if(l.actualType == null) l.staticType else l.actualType.widen}, 
            typeOf[HNil], l.tree, reify{HNil}.tree)
      }
      def init(c: Context): c.Expr[Any] = {
        import c.universe._
        /** We'd like to be able to write something like l.init = l.reverse.tail.reverse
         *  But we can't use reverse within reify as it belongs to the same compilation unit
         *  Therefore we leverage the reverseList function which yields a tree
         */
        val (rev, revType) = reverseList(c)(c.prefix)
        val (hType, tType) = revType match {
          case TypeRef(_, _, List(hType, tType)) => (hType, tType)
        }
        def genTree[H: WeakTypeTag, T <: ::[_, _]: WeakTypeTag] = {
          reverseList(c)(reify{c.Expr[::[H, T]](rev).splice.tail})._1
        }
        if(tType =:= typeOf[HNil])
          reify{HNil}
        else
          c.Expr(genTree(c.WeakTypeTag(hType), c.WeakTypeTag(tType)))
      }
    }




