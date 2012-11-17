package macroHList

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.macros.Context
import scala.reflect.ClassTag

import TypeOperators._
import Poly._

//object HList {

    trait HList {
      type Self <: HList
      def length: Int
      def isEmpty: Boolean
      def nonEmpty = !isEmpty
      def ::[E](e: E) = macro HList.prepend[Self, E]
      def :+[E](e: E) = macro HList.append[Self, E]
      def last: Any = macro HList.last[Self]
      def reverse: Any = macro HList.reverse[Self]
      def init: Any = macro HList.init[Self]
      /** Concatenate two HLists together.
       */
      def ++[L2 <: HList](l2: L2): Any = macro HList.++[Self, L2]
      def updated[E](i: Int, e: E): Any = macro HList.updated[Self, E]
      /** Tell whether the HList contains an element of type E or not.
       */
      def containsType[E]: Boolean = macro HList.containsType[Self, E]
      def contains[E](e: E): Boolean = macro HList.contains[Self, E]
      def diff[L2 <: HList](l2: L2): Any = macro HList.diff[Self, L2]
      /** Find first element of type E in this HList
       */
      def findType[E]: Any = macro HList.findType[Self, E]
      def find[E](f: E => Boolean): Any = macro HList.find[Self, E]
      /** Filter the HList so only elements of type E remains.
       *  If E is an existential type, it is used as witness.
       *  Combined with TypeOperators, it enables to build complex filters,
       *  such as conjunctions, disjunctions, etc
       */
      def filter[E]: Any = macro HList.filter[Self, E]
      /** The complementary of filter
       */
      def filterNot[E]: Any = macro HList.filterNot[Self, E]
      /** Transform this HList by applying the first applicable Poly1 function
       *  in the given HList as argument.
       */
      def map[HF <: HList](hf: HF) = macro HList.map[Self, HF]
      /** Flatten an HList of HLists to an HList.
       */
      def flatten = macro HList.flatten[Self]
      /** Get the i-th element of this HList. Only compile time known index is allowed.
       */
      def apply(i: Int): Any = macro HList.getIndex[Self]
      /** Returns the index of the first element of type E in this HList.
       */
      def indexOfType[E]: Int = macro HList.indexOfType[Self, E]
      def indexOfType[E](from: Int): Int = macro HList.indexOfTypeFrom[Self, E]
      def indexOf[E](e: E): Int = macro HList.indexOf[Self, E]
      def indexOf[E](e: E, from: Int): Int = macro HList.indexOfFrom[Self, E]
      /** Returns the index of the last element of type E in this HList.
       */
      def lastIndexOf[E]: Int = macro HList.lastIndexOf[Self, E]
      def lastIndexOf[E](end: Int): Int = macro HList.lastIndexOfEnd[Self, E]
      /** Take the first i elements of this HList. Only compile time known number is allowed
       *  as argument.
       */
      def take(i: Int): Any = macro HList.take[Self]
      def takeRight(i: Int): Any = macro HList.takeRight[Self]
      def drop(i: Int): Any = macro HList.drop[Self]
      def dropRight(i: Int): Any = macro HList.dropRight[Self]
      def takeWhile[E]: Any = macro HList.takeWhile[Self, E]
      def dropWhile[E]: Any = macro HList.dropWhile[Self, E]
      def span[E]: Any = macro HList.span[Self, E]
      def splitAt(i: Int): Any = macro HList.splitAt[Self]
      /** Unzip an HList of tuples to a tuple of HLists. Does not compile if the HList
       *  does not only contains tuples.
       */
      def unzip: Any = macro HList.unzip[Self]
      /** Zip two HLists to an HList of tuples.
       */
      def zip[L2 <: HList](l2: L2): Any = macro HList.zip[Self, L2]
      def zipAll[L2 <: HList, E1, E2](l2: L2, e1: E2, e2: E2): Any = macro HList.zipAll[Self, L2, E1, E2]
      def zipWithIndex: Any = macro HList.zipWithIndex[Self]
      /** Transform this HList to a standard List of the least upper bound type of the HList elements.
       */
      def toList: Any = macro HList.toList[Self]
      /** Transform this HList to a standard Array of the least upper bound type of the HList elements.
       */
      def toArray: Any = macro HList.toArray[Self]
      def startsWith[L2 <: HList](l2: L2): Boolean = macro HList.startsWith[Self, L2]
      def endsWith[L2 <: HList](l2: L2): Boolean = macro HList.endsWith[Self, L2]
      def count[HF <: HList](hf: HF): Int = macro HList.count[Self, HF]
      def mkString(start: String, sep: String, end: String): String
      def mkString(sep: String): String
      def mkString: String
      def toTuple: Any = macro HList.toTuple[Self]
      def toClass: Any = macro HList.toClass[Self]
      def reduce[F](f: F): Any = macro HList.reduce[Self, F]
    }

    case class ::[H, T <: HList](head: H, tail: T) extends HList {
      type Self = H :: T
      type Head = H
      def length = 1 + tail.length
      def isEmpty = false
      def mkString(start: String, sep: String, end: String) = 
        if(tail.isEmpty)
          start + head + end
        else
          start + head + sep + tail.mkString(sep) + end
      def mkString(sep: String) = mkString("", sep, "")
      def mkString = mkString("", "", "")
      override def toString = mkString("HList(", ", ", ")")
    }

    trait HNil extends HList {
      type Self = HNil
      def length = 0
      def isEmpty = true
      def mkString(start: String, sep: String, end: String) = start + end
      def mkString(sep: String) = ""
      def mkString = ""
      override def toString = "HNil"
    }

    case object HNil extends HNil {

    }

    class ListOps[A](l: List[A]) {
      //def toHList: Any = macro HList.toHList[List[A]]
    }
    //implicit def mkListOps[A](l: List[A]): ListOps[A] = new ListOps[A](l)



    object HList {
      /** Enriched macro context with HList useful reification functions
       */
       
      class HListContext[C <: Context](val c: C) {
        import c.universe._

        def isLiteral[T](expr: Expr[T]): Boolean = {
          c.echo(c.enclosingPosition, "isLiteral: tpe " + tpeFromExpr(expr) + " tree " + showRaw(expr.tree))
          expr.tree match {
            case Literal(_) => true
            case _ => false
          }
        }

        def tpeFromExpr[T](expr: Expr[T]): Type =
          if(expr.actualType == null) expr.staticType else expr.actualType.widen

        class AbsExpr(val tree: Tree, val tpe: Type) {
          def toExpr = c.Expr(tree)(c.WeakTypeTag(tpe))
          def apply(arg1: AbsExpr): AbsExpr = {
            def genApply[T: WeakTypeTag, R: WeakTypeTag]: AbsExpr =
              AbsExpr(reify(c.Expr[T => R](tree).apply(c.Expr[T](arg1.tree))))
            genApply(c.WeakTypeTag(tpe), c.WeakTypeTag(arg1.tpe))
          }
          def apply(arg1: AbsExpr, arg2: AbsExpr): AbsExpr = {
            def genApply[T1: WeakTypeTag, T2: WeakTypeTag, R: WeakTypeTag]: AbsExpr =
              AbsExpr(reify(c.Expr[(T1, T2) => R](tree).apply(c.Expr[T1](arg1.tree), c.Expr[T2](arg2.tree))))
            genApply(c.WeakTypeTag(tpe), c.WeakTypeTag(arg1.tpe), c.WeakTypeTag(arg2.tpe))
          }
          override def equals(expr: Any) = expr match {
            case AbsExpr(tree2, tpe2) => tpe.widen =:= tpe2.widen && tree.equalsStructure(tree2)
            case _ => false
          }
          override def hashCode = tree.hashCode + 41 * tpe.hashCode
          override def toString = "AbsExpr(" + show(tree) + ": " + tpe + ")"
        }
        object AbsExpr {
          def apply(tree: Tree, tpe: Type) = new AbsExpr(tree, tpe)
          def apply[T: WeakTypeTag](expr: Expr[T]): AbsExpr = new AbsExpr(expr.tree, tpeFromExpr(expr))
          def unapply(expr: AbsExpr): Option[(Tree, Type)] = Some((expr.tree, expr.tpe))
        }

        implicit def exprToAbs[T](expr: Expr[T]): AbsExpr = new AbsExpr(expr.tree, tpeFromExpr(expr))

        class TupleExpr(tree: Tree, tpe: Type) extends AbsExpr(tree, tpe) {
          def first: AbsExpr = tpe match {
            case TypeRef(_, tup, List(t1, t2)) => {
              def genFirst[T1: WeakTypeTag, T2: WeakTypeTag]: AbsExpr =
                AbsExpr(reify(c.Expr[(T1, T2)](tree).splice._1))
              genFirst(c.WeakTypeTag(t1), c.WeakTypeTag(t2))
            }
          }
          def second: AbsExpr = tpe match {
            case TypeRef(_, tup, List(t1, t2)) => {
              def genFirst[T1: WeakTypeTag, T2: WeakTypeTag]: AbsExpr =
                AbsExpr(reify(c.Expr[(T1, T2)](tree).splice._2))
              genFirst(c.WeakTypeTag(t1), c.WeakTypeTag(t2))
            }
          }
        }
        object TupleExpr {
          def apply(e1: AbsExpr, e2: AbsExpr): TupleExpr = {
            def genTuple[T1: WeakTypeTag, T2: WeakTypeTag]: TupleExpr =
              new TupleExpr(
                reify((c.Expr[T1](e1.tree).splice, c.Expr[T2](e2.tree).splice)).tree,
                weakTypeOf[(T1, T2)]
              )
            genTuple(c.WeakTypeTag(e1.tpe), c.WeakTypeTag(e2.tpe))
          }
          def apply(e: AbsExpr): TupleExpr = {
            def genTuple[T1: WeakTypeTag, T2: WeakTypeTag]: TupleExpr =
              new TupleExpr(
                reify((c.Expr[(T1, T2)](e.tree).splice._1, c.Expr[(T1, T2)](e.tree).splice._2)).tree,
                weakTypeOf[(T1, T2)]
              )
            c.echo(c.enclosingPosition, "TupleExpr " + e.tpe)
            e.tpe match {
              case TypeRef(_, _, List(t1, t2)) =>
                genTuple(c.WeakTypeTag(t1), c.WeakTypeTag(t2))
            }
          }
        }

        def genericMethodReturnType(m: Symbol, ts: List[Type]): Type = {
          appliedType(m.asMethod.typeSignature, ts) match {
            case MethodType(_, r) => r
          }
        }

        case class Poly1Expr(hf: ListExpr) {
 
          def apply(expr: AbsExpr): AbsExpr = {
        
            def test(tpe: Type): (Tree, Type, Type) = {
              val (arg1Tpe, argImplTpe) =
                tpe.baseType(typeOf[Poly1[Arg1, ArgImpl] forSome {type Arg1[X]; type ArgImpl[X]}].typeSymbol) match {
                case TypeRef(_, _, List(arg1, argImpl)) => (arg1, argImpl)
              }
              val argImplTTpe = appliedType(argImplTpe, List(expr.tpe)).normalize
              c.echo(c.enclosingPosition, "argImplTTpe " + argImplTTpe)
              val found = c.inferImplicitValue(argImplTTpe)
              (found, arg1Tpe, argImplTpe)
            }

            val poly = hf.find((t: Type) => {
              val (found, arg1Tpe, argImplTpe) = test(t)
              if(found == EmptyTree)
                false
              else
                expr.tpe <:< appliedType(arg1Tpe, List(WildcardType))
            })

            val (found, arg1Tpe, argImplTpe) = test(poly.tpe)

            def genApply[HF <: Poly1[Arg1, ArgImpl] forSome {type Arg1[X]; type ArgImpl[X]}: WeakTypeTag,
                         T: WeakTypeTag, Arg1T: WeakTypeTag, ArgImplT: WeakTypeTag] = {
              val reifee = reify{
                c.Expr[HF](poly.tree).splice.apply[T](c.Expr[Arg1T](expr.tree).splice)(c.Expr[ArgImplT](found).splice)
              }
              c.echo(c.enclosingPosition, "Poly1 reifee " + show(reifee))
              AbsExpr(reifee)
            }
            val tTpe = expr.tpe match {
              case TypeRef(_, sym, List(t)) => t
              case t if t =:= appliedType(arg1Tpe, List(t)) => t
            }
            genApply(c.WeakTypeTag(poly.tpe), c.WeakTypeTag(tTpe), c.WeakTypeTag(appliedType(arg1Tpe, List(tTpe))),
              c.WeakTypeTag(appliedType(argImplTpe, List(tTpe))))
          }
        }
        /*
        class Poly2Expr(hf: ListExpr) {
          
          def apply(expr1: AbsExpr, expr2: AbsExpr): AbsExpr = {
 
            def getPolyTypes(hfTpe: Type): (Type, Type, Type) = {
              val (arg1Tpe, arg2Tpe, argImplTpe) =
                hfTpe.baseType(typeOf[Poly2[Arg1, Arg2, ArgImpl] forSome {type Arg1[X]; type Arg2[X]; type ArgImpl[X]}].typeSymbol) match {
                case TypeRef(_, _, List(arg1, arg2, argImpl)) => (arg1, arg2, argImpl)
              }
              (arg1Tpe, arg2Tpe, argImplTpe)
            }

            val poly = hf.find((hfTpe: Type) => {
              val (arg1Tpe, arg2Tpe, argImplTpe) = getPolyTypes(hfTpe)
              val t1Tpe = expr1.tpe match {
                case TypeRef(_, sym,  
              }
              if(found == EmptyTree)
                false
              else
                expr1.tpe <:< appliedType(arg1Tpe, List(WildcardType)) &&
                expr2.tpe <:< appliedType(arg2Tpe, List(WildcardType))
            })

            val (found, arg1Tpe, arg2Tpe, argImplTpe) = test(poly.tpe)

            def genApply[HF <: Poly2[Arg1, Arg2, ArgImpl] forSome {
                         type Arg1[X]; type Arg2[X]; type ArgImpl[X]}: WeakTypeTag,
                         T: WeakTypeTag, Arg1T: WeakTypeTag, Arg2T: WeakTypeTag, ArgImplT: WeakTypeTag] = {
              val reifee = reify{
                c.Expr[HF](poly.tree).splice.apply[T](c.Expr[Arg1T](expr1.tree).splice,
                                                      c.Expr[Arg2T](expr2.tree).splice)(c.Expr[ArgImplT](found).splice)
              }
              c.info(c.enclosingPosition, "Poly2 reifee " + show(reifee), false)
              AbsExpr(reifee)
            }
            genApply(c.WeakTypeTag(poly.tpe), c.WeakTypeTag(tTpe), c.WeakTypeTag(appliedType(arg1Tpe, List(tTpe))),
              c.WeakTypeTag(appliedType(arg2Tpe, List(tTpe))), c.WeakTypeTag(appliedType(argImplTpe, List(tTpe))))
          }

        }
        */
        
        abstract class ListExpr(tree: Tree, tpe: Type) extends AbsExpr(tree, tpe) {
          def head: AbsExpr
          def tail: ListExpr
          def ::(e: AbsExpr): ListExpr
          def :+(e: AbsExpr): ListExpr
          def ++(l: ListExpr): ListExpr
          def reverse: ListExpr
          def last: AbsExpr
          def init: ListExpr
          def contains(t: Type): Expr[Boolean]
          def contains(e: AbsExpr): Expr[Boolean]
          def diff(l: ListExpr): ListExpr
          def filter(t: Type): ListExpr
          def filterNot(t: Type): ListExpr
          def find(t: Type): AbsExpr
          def find(f: Type => Boolean): AbsExpr
          def find(f: AbsExpr): AbsExpr
          def apply(i: Expr[Int]): AbsExpr
          def length: Expr[Int]
          def indexOf(t: Type): Expr[Int]
          def indexOf(t: Type, from: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int]
          def indexOf(e: AbsExpr): Expr[Int]
          def indexOf(e: AbsExpr, from: Expr[Int]): Expr[Int]
          def lastIndexOf(t: Type): Expr[Int]
          def lastIndexOf(t: Type, end: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int]
          def take(i: Expr[Int]): ListExpr
          def takeRight(i: Expr[Int]): ListExpr
          def drop(i: Expr[Int]): ListExpr
          def dropRight(i: Expr[Int]): ListExpr
          def takeWhile(t: Type): ListExpr
          def dropWhile(t: Type): ListExpr
          def span(t: Type): TupleExpr
          def splitAt(i: Expr[Int]): TupleExpr
          def unzip: TupleExpr
          def updated(i: Expr[Int], e: AbsExpr): ListExpr
          def zip(l: ListExpr): ListExpr
          def zipAll(l: ListExpr, e1: AbsExpr, e2: AbsExpr): ListExpr
          def zipWithIndex: ListExpr
          def toList: AbsExpr
          def toArray: AbsExpr
          def tupled: AbsExpr
          def unify: ListExpr
          def startsWith(l: ListExpr): Expr[Boolean]
          def endsWith(l: ListExpr): Expr[Boolean]
          def map(hf: ListExpr): ListExpr
          def flatten: ListExpr
          def flatMap(hf: ListExpr): ListExpr
          def foldLeft(e: AbsExpr)(l: ListExpr): AbsExpr
          def foldRight(e: AbsExpr)(l: ListExpr): AbsExpr
          def reduceLeft(l: ListExpr): AbsExpr
          def reduceRight(l: ListExpr): AbsExpr
          def reduce(f: AbsExpr): AbsExpr = {
            val res = treeBuild.mkMethodCall(f.tree, trees)
            AbsExpr(c.Expr(res))
          }
          def count(hf: ListExpr): Expr[Int]
          def toTuple: AbsExpr
          def toClass: AbsExpr
          def trees: List[Tree]
          def tpes: List[Type]
        }

        object ListExpr {
          def apply[T: WeakTypeTag](expr: Expr[T]): ListExpr = {
            val tpe = tpeFromExpr(expr)
            if(tpe <:< typeOf[HNil])
              HNilExpr
            else if(tpe <:< typeOf[_ :: _])
              HListExpr(expr.tree, tpe)
            else
              sys.error("Unknown HList type " + tpe)
          }
          def apply(tree: Tree, tpe: Type): ListExpr = {
            if(tpe <:< typeOf[HNil])
              HNilExpr
            else if(tpe <:< typeOf[_ :: _])
              HListExpr(tree, tpe)
            else
              sys.error("Unknown HList type " + tpe)
          }
          def apply(expr: AbsExpr): ListExpr = ListExpr(expr.tree, expr.tpe)
        }

        class HListExpr(tree: Tree, tpe: Type) extends ListExpr(tree, tpe) {

          val (headTpe, tailTpe) = tpe match {
            case TypeRef(_, cons, List(h, t)) => (h ,t) 
          }

          def head: AbsExpr = {
             def genHead[H: WeakTypeTag, T <: HList: WeakTypeTag]: AbsExpr =
               AbsExpr(reify(c.Expr[H :: T](tree).splice.head))
             genHead(c.WeakTypeTag(headTpe), c.WeakTypeTag(tailTpe))
          }

          def tail: ListExpr = {
             def genTail[H: WeakTypeTag, T <: HList: WeakTypeTag]: ListExpr =
               ListExpr(reify(c.Expr[H :: T](tree).splice.tail))
             genTail(c.WeakTypeTag(headTpe), c.WeakTypeTag(tailTpe))
          }

          def ::(e: AbsExpr): ListExpr = {
             def genCons[E: WeakTypeTag, L <: HList: WeakTypeTag]: ListExpr =
               ListExpr(reify(new ::(c.Expr[E](e.tree).splice, c.Expr[L](tree).splice)))
             genCons(c.WeakTypeTag(e.tpe), c.WeakTypeTag(tpe))
          }

          def reverse: ListExpr = tail.reverse :+ head

          def last: AbsExpr = reverse.head

          def init: ListExpr = reverse.tail.reverse

          def :+(e: AbsExpr) = head :: (tail :+ e)

          def ++(l: ListExpr) = init ++ (last :: l)
          
          def typeLookup(t: Type, r: Type): Tree = {
            def replaceWildcard(t: Type, r: Type): Type = t match {
              case ExistentialType(List(t1, _*), TypeRef(pre, sym, l)) => TypeRef(pre, sym, l.map(tt =>
                if(tt =:= t1.asType.toType)
                  r
                else
                  replaceWildcard(tt, r)
              ))
              case _ => t
            }
            val hiding =
              if(t.typeConstructor.takesTypeArgs)
                replaceWildcard(t, r)
              else
                appliedType(typeOf[_ <:< _], List(r, t))
            c.echo(c.enclosingPosition, "Looking for " + hiding)
            c.inferImplicitValue(hiding)
          }

          def contains(t: Type): Expr[Boolean] = {
            val found = typeLookup(t, head.tpe)
            if(found != EmptyTree)
              reify(true)
            else
              tail.contains(t)
          }
          def contains(e: AbsExpr): Expr[Boolean] = {
            reify(c.Expr[Any](head.tree).splice == c.Expr[Any](e.tree).splice ||
                  c.Expr[Boolean](tail.contains(e).tree).splice)
          }
          // if we have diff, we have distinct
          def diff(l: ListExpr): ListExpr = ???
          /*{
            if()
              tail.diff(l)
            else
              head :: tail.diff(l)
          }*/

          // For internal purposes
          def find(f: Type => Boolean): AbsExpr = {
            if(f(head.tpe))
              head
            else
              tail.find(f)
          }

          def find(t: Type): AbsExpr =
            find((t: Type) => {
              lazy val res = typeLookup(t, head.tpe) == EmptyTree
              res
            })

          def find(f: AbsExpr): AbsExpr = {
            val tpe = f.tpe match {
              case TypeRef(_, _, List(e, b)) => e
            }
            def genFind[E: WeakTypeTag] =
              AbsExpr(reify(c.Expr[List[E]](filter(tpe).toList.tree).splice.find(c.Expr[E => Boolean](f.tree).splice)))
            genFind(c.WeakTypeTag(tpe))
          }

          def filter(t: Type): ListExpr = {
            val found = typeLookup(t, head.tpe) 
            if(found != EmptyTree)
              head :: tail.filter(t)
            else
              tail.filter(t)
          }
          
          def filterNot(t: Type): ListExpr = {
            val found = typeLookup(t, head.tpe)
            if(found == EmptyTree)
              head :: tail.filterNot(t)
            else
              tail.filterNot(t)   
          }

          def apply(i: Expr[Int]): AbsExpr = {
            if(c.eval(c.Expr[Int](c.resetAllAttrs(i.tree.duplicate))) == 0)
              head
            else
              tail.apply(reify(i.splice - 1))
          }
  
          def indexOf(t: Type): Expr[Int] = indexOf(t, reify(0))

          def indexOf(t: Type, from: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int] = {
            if(c.eval(c.Expr[Int](c.resetAllAttrs(reify(offset.splice - from.splice).tree))) >= 0) {
              val found = typeLookup(t, head.tpe)
              if(found != EmptyTree)
                return offset
            }
            tail.indexOf(t, from, reify(offset.splice + 1))
          }

          def indexOf(e: AbsExpr, from: Expr[Int]): Expr[Int] = {
            def genIndexOf[L: WeakTypeTag, E: WeakTypeTag] =
              reify(c.Expr[List[L]](toList.tree).splice.indexOf(c.Expr[E](e.tree).splice, from.splice))
            //c.echo(c.enclosingPosition, "e.tpe " + e.tpe)
            genIndexOf(c.WeakTypeTag(lub(tpes)), c.WeakTypeTag(e.tpe))
          }

          def indexOf(e: AbsExpr): Expr[Int] = indexOf(e, reify(0))

          def lastIndexOf(t: Type, end: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int] = {
            val i = reverse.indexOf(t, reify(length.splice - 1 - end.splice), offset)
            reify{
              if(i.splice >= 0)
                length.splice - 1 - i.splice
              else
                i.splice
            }
          }

          def lastIndexOf(t: Type): Expr[Int] = lastIndexOf(t, reify(length.splice - 1))

          def length: Expr[Int] = reify(1 + tail.length.splice)

          def take(i: Expr[Int]): ListExpr = {
            if(c.eval(c.Expr[Int](c.resetAllAttrs(i.tree.duplicate))) <= 0)
              HNilExpr
            else
              head :: tail.take(reify(i.splice - 1))
          }

          def takeRight(i: Expr[Int]): ListExpr = reverse.take(i).reverse 

          def drop(i: Expr[Int]): ListExpr = takeRight(reify(length.splice - i.splice))

          def dropRight(i: Expr[Int]): ListExpr = take(reify(length.splice - i.splice))

          def takeWhile(t: Type): ListExpr = {
            val found = typeLookup(t, head.tpe)
            if(found != EmptyTree)
              head :: tail.takeWhile(t)
            else
              HNilExpr
          }

          def dropWhile(t: Type): ListExpr = {
            val found = typeLookup(t, head.tpe)
            if(found != EmptyTree)
              tail.dropWhile(t)
            else
              this
          }

          def span(t: Type): TupleExpr = TupleExpr(takeWhile(t), dropWhile(t))

          def splitAt(i: Expr[Int]): TupleExpr = TupleExpr(take(i), drop(i))

          def unzip: TupleExpr = {
            val headTup = TupleExpr(head)
            val tailTup = tail.unzip
            TupleExpr(headTup.first :: ListExpr(tailTup.first), 
                      headTup.second :: ListExpr(tailTup.second))
          }

          def updated(i: Expr[Int], e: AbsExpr): ListExpr = {
            if(c.eval(c.Expr[Int](c.resetAllAttrs(i.tree.duplicate))) == 0)
              e :: tail
            else
              head :: tail.updated(reify(i.splice - 1), e)
          }

          def zip(l: ListExpr): ListExpr = {
            if(l == HNilExpr)
              HNilExpr
            else
              TupleExpr(head, l.head) :: tail.zip(l.tail)
          }

          def zipAll(l: ListExpr, e1: AbsExpr, e2: AbsExpr): ListExpr = {
            if(l == HNilExpr)
              TupleExpr(head, e1) :: tail.zipAll(HNilExpr, e1, e2)
            else
              TupleExpr(head, l.head) :: tail.zipAll(l.tail, e1, e2)
          }

          protected def reverseIndexes: ListExpr = tail match {
            case HNilExpr => AbsExpr(reify(0)) :: HNilExpr
            case hltail @ HListExpr(_, _) => reify(length.splice - 1) :: hltail.reverseIndexes
          } 

          def zipWithIndex: ListExpr = zip(reverseIndexes.reverse)
          
          /*
          protected def tpes: List[Type] = tail match {
            case HNilExpr => List(head.tpe)
            case hltail @ HListExpr(_, _) => head.tpe :: hltail.tpes
          }
          */

          def toList: AbsExpr = {
            def genList[A: WeakTypeTag]: AbsExpr =
              AbsExpr(reify(c.Expr[A](head.tree).splice :: c.Expr[List[A]](tail.toList.tree).splice))
            genList(c.WeakTypeTag(lub(tpes)))
          }

          def toArray: AbsExpr = {
            val found = c.inferImplicitValue(appliedType(typeOf[ClassTag[_]].typeConstructor, List(lub(tpes))))
            if(found == EmptyTree)
              sys.error("No ClassTag found for " + lub(tpes))
            def genArray[A: WeakTypeTag]: AbsExpr =
              AbsExpr(reify(c.Expr[List[A]](toList.tree).splice.toArray[A](c.Expr[ClassTag[A]](found).splice)))
            genArray(c.WeakTypeTag(lub(tpes)))
          }

          def tupled: AbsExpr = toTuple

          def unify: ListExpr = ??? // reify(toList.splice).toHList

          def startsWith(l: ListExpr): Expr[Boolean] = {
            if(l == HNilExpr)
              reify(true)
            else
              reify(c.Expr[Any](head.tree).splice == c.Expr[Any](l.head.tree).splice &&
                    c.Expr[Boolean](tail.startsWith(l.tail).tree).splice)
          }

          def endsWith(l: ListExpr): Expr[Boolean] = reverse.startsWith(l.reverse)

          /** find applicable function in hf HList for each element of HList
           *  e.g. for element of type X find Poly1[Arg1] such that
           *  X <:< Arg1[_]
           */

          def map(hf: ListExpr): ListExpr = Poly1Expr(hf).apply(head) :: tail.map(hf)

          def count(hf: ListExpr): Expr[Int] =
            reify(c.Expr[List[Boolean]](map(hf).toList.tree).splice.map(b => if(b) 1 else 0).reduceLeft(_ + _))

          def flatten: ListExpr = ListExpr(head.tree, head.tpe) ++ tail.flatten

          def flatMap(hf: ListExpr): ListExpr = map(hf).flatten

          def reduceLeft(f: ListExpr): AbsExpr = {
            if(tail == HNilExpr)
              head
            else {
              def reduceFun(t1: Type, t2: Type): Type =
                appliedType(typeOf[_ => _], List(
                  appliedType(typeOf[(_, _)], List(t1, t2)),
                  definitions.AnyTpe)
                )
              reduceLeft(f.find(reduceFun(head.tpe, tail.head.tpe)).apply(head, tail.head) :: tail.tail)
            }
          }

          def reduceRight(f: ListExpr): AbsExpr = {
            if(tail == HNilExpr)
              head
            else {
              def reduceFun(t1: Type, t2: Type): Type =
                 appliedType(typeOf[_ => _], List(
                  appliedType(typeOf[(_, _)], List(t1, t2)),
                  definitions.AnyTpe)
                )
              reduceRight(f.find(reduceFun(last.tpe, init.last.tpe)).apply(last, init.last) :: init.init)
            }
          }

          //def reduce(f: AbsExpr): AbsExpr = 

          def foldLeft(e: AbsExpr)(f: ListExpr): AbsExpr = (e :: this).reduceLeft(f)

          def foldRight(e: AbsExpr)(f: ListExpr): AbsExpr = (this :+ e).reduceRight(f)

          def trees: List[Tree] = head.tree :: tail.trees

          def tpes: List[Type] = head.tpe :: tail.tpes

          def toTuple: AbsExpr = {
            val length = c.eval(c.Expr[Int](this.length.tree))
            // Get tuple symbol we're interested in
            val tupSym = rootMirror.staticModule("scala.Tuple" + length)
            val tupTree = treeBuild.mkMethodCall(tupSym, newTermName("apply"), tpes, trees)
            AbsExpr(c.Expr(tupTree))
          }

          /** Recursive class building based on companion object apply method
          *  which is supposed to be stored at the HList head, while the
          *  arguments constitute the tail.
          */
          def toClass: AbsExpr = {
            val applySym = head.tpe.member(newTermName("apply")).asMethod
            def genArgTrees(l: ListExpr): List[Tree] = {
              if(l == HNilExpr)
                Nil
              else {
                if(!(l.head.tpe <:< typeOf[HList]))
                  l.head.tree :: genArgTrees(l.tail)
                else
                  ListExpr(l.head).toClass.tree :: genArgTrees(l.tail)
              }
            }
            val argTrees = genArgTrees(tail)
            c.info(NoPosition, "Generated arg trees:\n" + argTrees.mkString("\n"),
              System.getProperty("force", "false").toBoolean)
            AbsExpr(treeBuild.mkMethodCall(applySym, argTrees), applySym.returnType)
          }


        }
        object HListExpr {
          def apply[T](expr: Expr[T]): HListExpr = new HListExpr(expr.tree, tpeFromExpr(expr))
          def apply(tree: Tree, tpe: Type): HListExpr = new HListExpr(tree, tpe)
          def unapply(hl: HListExpr): Option[(Tree, Type)] = Some((hl.tree, hl.tpe))
        }
        
        implicit def exprToHList[H, T <: HList](expr: Expr[H :: T]): HListExpr =
          HListExpr(expr.tree, tpeFromExpr(expr))
        /* 
        */

        case object HNilExpr extends ListExpr(reify(HNil).tree, typeOf[HNil]) {
          def ::(e: AbsExpr): ListExpr = {
            def genCons[E: WeakTypeTag]: ListExpr =
              HListExpr(reify(new ::(c.Expr[E](e.tree).splice, HNil)))
            genCons(c.WeakTypeTag(e.tpe))
          }
          def :+(e: AbsExpr): ListExpr = ::(e)
          def ++(l: ListExpr): ListExpr = l
          def tail: ListExpr = sys.error("Tail of HNil does not exist")
          def head: AbsExpr = sys.error("Head of HNil does not exist")
          def reverse: ListExpr = HNilExpr
          def last: AbsExpr = sys.error("Last of HNil does not exist")
          def init: ListExpr = sys.error("Init of HNil does not exist")
          def contains(t: Type): Expr[Boolean] = reify(false)
          def contains(e: AbsExpr): Expr[Boolean] = reify(false)
          def diff(l: ListExpr): ListExpr = HNilExpr
          def find(t: Type): AbsExpr = sys.error("Element of type " + t + " not found")
          def find(f: Type => Boolean): AbsExpr = sys.error("Element not found")
          def find(f: AbsExpr): AbsExpr = reify(None)
          def filter(t: Type): ListExpr = HNilExpr
          def filterNot(t: Type): ListExpr = HNilExpr
          def apply(i: Expr[Int]): AbsExpr = sys.error("HNil has no element")
          def indexOf(t: Type): Expr[Int] = reify(-1)
          def indexOf(t: Type, from: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int] = reify(-1)
          def indexOf(e: AbsExpr): Expr[Int] = reify(-1)
          def indexOf(e: AbsExpr, from: Expr[Int]) = reify(-1)
          def lastIndexOf(t: Type): Expr[Int] = reify(-1)
          def lastIndexOf(t: Type, from: Expr[Int], offset: Expr[Int] = reify(0)): Expr[Int] = reify(-1)
          def length: Expr[Int] = reify(0)
          def take(i: Expr[Int]): ListExpr = HNilExpr
          def takeRight(i: Expr[Int]): ListExpr = HNilExpr
          def drop(i: Expr[Int]): ListExpr = HNilExpr
          def dropRight(i: Expr[Int]): ListExpr = HNilExpr
          def takeWhile(t: Type): ListExpr = HNilExpr
          def dropWhile(t: Type): ListExpr = HNilExpr
          def unzip: TupleExpr = TupleExpr(HNilExpr, HNilExpr)
          def updated(i: Expr[Int], e: AbsExpr): ListExpr = sys.error("HNil can not be updated")
          def span(t: Type): TupleExpr = TupleExpr(HNilExpr, HNilExpr)
          def splitAt(i: Expr[Int]): TupleExpr = TupleExpr(HNilExpr, HNilExpr)
          def zip(l: ListExpr): ListExpr = HNilExpr
          def zipAll(l: ListExpr, e1: AbsExpr, e2: AbsExpr): ListExpr =
            if(l == HNilExpr)
              HNilExpr
            else
              TupleExpr(e2, l.head) :: zipAll(l.tail, e1, e2)
          def zipWithIndex: ListExpr = HNilExpr
          def toList: AbsExpr = AbsExpr(reify(Nil))
          def toArray: AbsExpr = sys.error("HNil can not convert to Array")
          def tupled: AbsExpr = sys.error("HNil can not be tupled")
          def unify: ListExpr = HNilExpr
          def startsWith(l: ListExpr): Expr[Boolean] =
            if(l == HNilExpr) reify(true) else reify(false)
          def endsWith(l: ListExpr): Expr[Boolean] =
            if(l == HNilExpr) reify(true) else reify(false)
          def map(hf: ListExpr): ListExpr = HNilExpr
          def flatten: ListExpr = HNilExpr
          def flatMap(hf: ListExpr): ListExpr = HNilExpr
          def foldLeft(e: AbsExpr)(f: ListExpr): AbsExpr = e
          def foldRight(e: AbsExpr)(f: ListExpr): AbsExpr = e
          def reduceLeft(f: ListExpr): AbsExpr = sys.error("HNil can not be reduced")
          def reduceRight(f: ListExpr): AbsExpr = sys.error("HNil can not be reduced")
          def count(hf: ListExpr): Expr[Int] = reify(0)
          def toTuple: AbsExpr = sys.error("HNil can not be converted to a tuple")
          def toClass: AbsExpr = sys.error("HNil can not be converted to a class instance")
          def trees: List[Tree] = Nil
          def tpes: List[Type] = Nil
          override def toString = "HNilExpr"
        }

        def fromTraversable(list: AbsExpr): ListExpr = {
          def genSize[L <: Traversable[_]: WeakTypeTag] =
            reify(c.Expr[L](list.tree).splice.size)
          val size = genSize(c.WeakTypeTag(list.tpe))
          if(c.eval(c.Expr[Int](c.resetAllAttrs(size.tree.duplicate))) > 0) {
            def genList[L <: Traversable[_]: WeakTypeTag] =
              AbsExpr(reify(c.Expr[L](list.tree).splice.head)) ::
              fromTraversable(AbsExpr(reify(c.Expr[L](list.tree).splice.tail)))
                       //reify(c.Expr[Int](size.tree).splice - 1))
            genList(c.WeakTypeTag(list.tpe))
          }
          else
            HNilExpr
        }

        /**
         *  TODO: once SI-5923 is fixed, an implicit conversion function can be defined on Tuples ;))
         */

        def fromTuple(tup: AbsExpr): ListExpr = {
          // get the tuple symbol tupleX
          val tupSymOption = tup.tpe.baseClasses.find(_.fullName.matches("scala.Tuple[0-9]+"))
          if(!tupSymOption.isDefined) {
            c.info(NoPosition, "Not a tuple, returning single element ListExpr",
              System.getProperty("force", "false").toBoolean)
            return tup :: HNilExpr
          }
          val tupSym = tupSymOption.get
          c.info(NoPosition, "Found tuple type: " + tupSym, System.getProperty("force", "false").toBoolean)
          // get the tuple arity
          val tupArity = tupSym.fullName.drop("scala.Tuple".length).toInt
          c.info(NoPosition, "Tuple has arity: " + tupArity, System.getProperty("force", "false").toBoolean)
          // get tuple element trees
          val tupTrees = (1 to tupArity).map(i =>
            treeBuild.mkAttributedSelect(tup.tree, tup.tpe.member(newTermName("_" + i))))
          c.info(NoPosition, "Building tuple trees:\n" + tupTrees.mkString("\n"),
            System.getProperty("force", "false").toBoolean)
          // get tuple element types
          val tupTpes = tup.tpe match {
            case TypeRef(_, _, tpes) => tpes
          }
          // transform tuple trees and types to AbsExpr and build the ListExpr
          val res =
          tupTrees.zip(tupTpes).map{case ((expr, tpe)) => AbsExpr(expr, tpe)}.foldRight(HNilExpr: ListExpr)(_ :: _)
          c.info(NoPosition, "Generated ListExpr: " + res, System.getProperty("force", "false").toBoolean)
          res
        }

        /** Generate an HList with class constructor at its head and constructor arguments as tail.
         *  TODO: Recursive on args, as long as it can, i.e. a unique apply and unapply function is found
         *  in companion object.
         *
         *  FIXME: Crashes the compiler if function with wildcard is passed as argument
         */

        def fromClass(clazz: AbsExpr, unapply0: AbsExpr): ListExpr = {
          // "reducing" functions to values
          def normUnapply[F: WeakTypeTag] =
            AbsExpr(reify{val unapply = c.Expr[F](unapply0.tree).splice; unapply})
          val unapply = normUnapply(c.WeakTypeTag(unapply0.tpe))
          c.info(NoPosition, "Normalized unapply to: " + unapply, System.getProperty("force", "false").toBoolean)
          // Unapplying the clazz
          def genArgs[C: WeakTypeTag, T: WeakTypeTag] =
            AbsExpr(reify(
              c.Expr[C => Option[T]](unapply.tree).splice.apply(c.Expr[C](clazz.tree).splice).get
            ))
          val tupTpe = unapply.tpe match {
            case TypeRef(_, _, fun) => fun.last match {
              case TypeRef(_, _, List(t)) => t
            }
          }
          val args = genArgs(c.WeakTypeTag(clazz.tpe), c.WeakTypeTag(tupTpe))
          c.info(NoPosition, "Found class unapply args: " + args, System.getProperty("force", "false").toBoolean)
          val argsList = fromTuple(args)
          c.info(NoPosition, "Converted tuple to ListExpr: " + argsList,
            System.getProperty("force", "false").toBoolean)
          val clazzExpr = AbsExpr(c.Expr(treeBuild.mkAttributedIdent(clazz.tpe.typeSymbol.companionSymbol)))
          clazzExpr :: argsList
        }

        /** FIXME: case class with a single field
         */
        
        def fromClass(clazz: AbsExpr): Option[ListExpr] = {
          // getting companion object symbol
          val companionSym = clazz.tpe.typeSymbol.companionSymbol
          if(companionSym == NoSymbol) {
            c.info(NoPosition, "Haven't found a companion object for: " + clazz.tpe,
              System.getProperty("force", "false").toBoolean)
            return None
          }
          // getting companion object type
          val companionTpe = SingleType(NoPrefix, clazz.tpe.typeSymbol.companionSymbol)
          // gettings unapply methods of the companion object
          val unapplyMethod = companionTpe.member(newTermName("unapply"))
          if(unapplyMethod == NoSymbol) {
             c.info(NoPosition, "Haven't found an unapply method for: " + clazz.tpe,
              System.getProperty("force", "false").toBoolean) 
            return None
          }
          c.info(NoPosition, "Found unapply method with type signature " + unapplyMethod.asMethod.typeSignature,
            System.getProperty("force", "false").toBoolean)
          val argsOption = c.Expr(treeBuild.mkMethodCall(unapplyMethod, List(clazz.tree)))
          val clazzTypeParams = clazz.tpe match {
            case TypeRef(_, _, params) => params
            case _ => Nil
          }
          c.info(NoPosition, "Found class parameters: " + clazzTypeParams,
            System.getProperty("force", "false").toBoolean)
          val argsList = unapplyMethod.asMethod.returnType match {
              case TypeRef(_, _, List(returnTpe)) => { // This is an option
                def genArgs[T: WeakTypeTag] =
                  AbsExpr(reify(c.Expr[Option[T]](argsOption.tree).splice.get))
                val args = genArgs(c.WeakTypeTag(appliedType(returnTpe, clazzTypeParams)))
                fromTuple(args)
              }
              case t if t =:= typeOf[Boolean] => HNilExpr
          }
          //c.info(NoPosition, "Generated args from unapply: " + args,
          //  System.getProperty("force", "false").toBoolean)
          //val argsList = fromTuple(args)
          // Attempt to further hlistify argsList
          def genArgsClass(l: ListExpr): ListExpr = {
            if(l == HNilExpr)
              l
            else {
              fromClass(l.head).getOrElse(l.head) :: genArgsClass(l.tail)
            }
          }
          val clazzExpr = AbsExpr(c.Expr(treeBuild.mkAttributedIdent(clazz.tpe.typeSymbol.companionSymbol)))
          Some(clazzExpr :: genArgsClass(argsList))
        }


      }

      def hListContext(c: Context) = new HListContext[c.type](c)

      /** Now mapping functions working on AbsExpr to macro implementations
       *  which are working on plain Expr
       */
      
      def prepend[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(e: c.Expr[E]) = {
        val hl = hListContext(c)
        (hl.AbsExpr(e) :: hl.ListExpr(c.Expr[L](c.prefix.tree))).toExpr     
      }

      def append[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(e: c.Expr[E]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)) :+ hl.AbsExpr(e)).toExpr
      }

      def reverse[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).reverse.toExpr

      def last[L <: HList: c.WeakTypeTag](c: Context): c.Expr[Any] =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).last.toExpr

      def init[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).init.toExpr

      def ++[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag](c: Context)(l2: c.Expr[L2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).++(hl.ListExpr(l2))).toExpr
      }

      def updated[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(i: c.Expr[Int], e: c.Expr[E]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).updated(i, hl.AbsExpr(e))).toExpr
      }

      def containsType[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).contains(c.weakTypeOf[E])

      def contains[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(e: c.Expr[E]) = {
        val hl = hListContext(c)
        hl.ListExpr(c.Expr[L](c.prefix.tree)).contains(hl.AbsExpr(e))
      }

      def diff[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag](c: Context)(l2: c.Expr[L2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).diff(hl.ListExpr(l2))).toExpr     
      }

      def findType[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).find(c.weakTypeOf[E]).toExpr

      def find[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(f: c.Expr[E => Boolean]) = {
        val hl = hListContext(c)
        hl.ListExpr(c.Expr[L](c.prefix.tree)).find(hl.AbsExpr(f)).toExpr
      }

      def filter[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).filter(c.weakTypeOf[E]).toExpr

      def filterNot[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).filterNot(c.weakTypeOf[E]).toExpr

      def map[L <: HList: c.WeakTypeTag, HF <: HList: c.WeakTypeTag](c: Context)(hf: c.Expr[HF]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).map(hl.ListExpr(hf))).toExpr
      }

      def flatten[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).flatten.toExpr

      def getIndex[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).apply(i).toExpr

      def indexOfType[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).indexOf(c.weakTypeOf[E])

      def indexOfTypeFrom[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(from: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).indexOf(c.weakTypeOf[E], from)

      def indexOf[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(e: c.Expr[E]) = {
        val hl = hListContext(c)
        hl.ListExpr(c.Expr[L](c.prefix.tree)).indexOf(hl.AbsExpr(e))
      }

      def indexOfFrom[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(e: c.Expr[E], from: c.Expr[Int]) = {
        val hl = hListContext(c)
        hl.ListExpr(c.Expr[L](c.prefix.tree)).indexOf(hl.AbsExpr(e), from)
      }

      def lastIndexOf[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).lastIndexOf(c.weakTypeOf[E])

      def lastIndexOfEnd[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context)(end: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).lastIndexOf(c.weakTypeOf[E], end)

      def take[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).take(i).toExpr

      def takeRight[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).takeRight(i).toExpr

      def drop[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).drop(i).toExpr

      def dropRight[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).dropRight(i).toExpr

      def takeWhile[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).takeWhile(c.weakTypeOf[E]).toExpr

      def dropWhile[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).dropWhile(c.weakTypeOf[E]).toExpr

      def span[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).span(c.weakTypeOf[E]).toExpr

      def splitAt[L <: HList: c.WeakTypeTag](c: Context)(i: c.Expr[Int]) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).splitAt(i).toExpr

      def unzip[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).unzip.toExpr

      def zip[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag](c: Context)(l2: c.Expr[L2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).zip(hl.ListExpr(l2))).toExpr     
      }

      def zipAll[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag,
                 E1: c.WeakTypeTag, E2: c.WeakTypeTag](c: Context)(
                 l2: c.Expr[L2], e1: c.Expr[E1], e2: c.Expr[E2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).zipAll(hl.ListExpr(l2), hl.AbsExpr(e1), hl.AbsExpr(e2))).toExpr     
      }

      def zipWithIndex[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).zipWithIndex.toExpr

      def toList[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).toList.toExpr

      def toArray[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).toArray.toExpr

      def toTuple[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).toTuple.toExpr

      def toClass[L <: HList: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).toClass.toExpr

      def startsWith[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag](c: Context)(l2: c.Expr[L2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).startsWith(hl.ListExpr(l2)))   
      }

      def endsWith[L <: HList: c.WeakTypeTag, L2 <: HList: c.WeakTypeTag](c: Context)(l2: c.Expr[L2]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).endsWith(hl.ListExpr(l2)))   
      }

      def count[L <: HList: c.WeakTypeTag, HF <: HList: c.WeakTypeTag](c: Context)(hf: c.Expr[HF]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).count(hl.ListExpr(hf)))   
      }

      def reduce[L <: HList: c.WeakTypeTag, F: c.WeakTypeTag](c: Context)(f: c.Expr[F]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).reduce(hl.AbsExpr(f))).toExpr 
      }
     
      /** Converts a tuple of any arity to an HList.
       *  TODO: once SI-5923 is fixed, an implicit conversion function can be defined on Tuples ;))
       */

      def fromTuple[T](tup: T) = macro fromTupleImpl[T]

      def fromTupleImpl[T: c.WeakTypeTag](c: Context)(tup: c.Expr[T]) = {
        val hl = hListContext(c)
        hl.fromTuple(hl.AbsExpr(tup)).toExpr
      }

      def fromClass[C, U](clazz: C, unapply: U) = macro fromClassImpl[C, U]

      def fromClassImpl[C: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(clazz: c.Expr[C],
        unapply: c.Expr[U]) = {
        val hl = hListContext(c)
        hl.fromClass(hl.AbsExpr(clazz), hl.AbsExpr(unapply)).toExpr 
      }

      def fromClass[C](clazz: C) = macro fromClassDirectImpl[C]

      def fromClassDirectImpl[C: c.WeakTypeTag](c: Context)(clazz: c.Expr[C]) = {
        val hl = hListContext(c)
        hl.fromClass(hl.AbsExpr(clazz)).get.toExpr
      }

      def fromTraversable[T <: Traversable[_]](list: T) = macro fromTraversableImpl[T]

      def fromTraversableImpl[T <: Traversable[_]: c.WeakTypeTag](c: Context)(list: c.Expr[T]) = {
        val hl = hListContext(c)
        hl.fromTraversable(hl.AbsExpr(list)).toExpr
      }

  }

//}
