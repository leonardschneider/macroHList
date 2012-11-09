package macroHList

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.existentials
import scala.language.higherKinds
import scala.reflect.macros.Context

import TypeOperators._

    trait HList {
      def length: Int
      def isEmpty: Boolean
      def nonEmpty = !isEmpty
    }

    case class ::[H, T <: HList](head: H, tail: T) extends HList {
      def ::[H](h: H) = macroHList.::(h, this)
      def :+[E](e: E) = macro HList.append[H :: T, E]
      def last: Any = macro HList.last[H :: T]
      def reverse: Any = macro HList.reverse[H :: T]
      def init: Any = macro HList.init[H :: T]
      def find[E]: Any = macro HList.find[H :: T, E]
      def filter[E]: Any = macro HList.filter[H :: T, E]
      def filterNot[E]: Any = macro HList.filterNot[H :: T, E]
      def map[HF <: HList](hf: HF) = macro HList.map[H :: T, HF]
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
      /** Enriched macro context with HList useful reification functions
       */
       
      class HListContext[C <: Context](val c: C) {
        import c.universe._

        def isLiteral(tree: Tree): Boolean = tree match {
          case Literal(_) => true
          case _ => false
        }

        def tpeFromExpr[T](expr: Expr[T]): Type =
          if(expr.actualType == null) expr.staticType else expr.actualType.widen

        //implicit def tpeToWeakTypeTag[T](tpe: Type): WeakTypeTag[T] = c.WeakTypeTag[T](tpe)

        class AbsExpr(val tree: Tree, val tpe: Type) {
          def toExpr = {
            def genExpr[T: WeakTypeTag]: Expr[T] = c.Expr[T](tree)
            genExpr(c.WeakTypeTag(tpe))
          }
          def splice = toExpr.splice
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
        }
        object AbsExpr {
          def apply(tree: Tree, tpe: Type) = new AbsExpr(tree, tpe)
          def apply[T: WeakTypeTag](expr: Expr[T]): AbsExpr = new AbsExpr(expr.tree, tpeFromExpr(expr))
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
              new TupleExpr(reify((c.Expr[T1](e1.tree).splice, c.Expr[T2](e2.tree).splice)).tree, weakTypeOf[(T1, T2)])
            genTuple(c.WeakTypeTag(e1.tpe), c.WeakTypeTag(e2.tpe))
          }
        }

        def genericMethodReturnType(m: Symbol, ts: List[Type]): Type = {
          appliedType(m.asMethod.typeSignature, ts) match {
            case MethodType(_, r) => r
          }
        }

        class Poly1Expr(tree: Tree, tpe: Type) extends AbsExpr(tree, tpe) {
          override def apply(expr: AbsExpr): AbsExpr = {
            def genApply[HF <: Poly1[Arg1] forSome {type Arg1[X]}: WeakTypeTag, T: WeakTypeTag, Arg1T: WeakTypeTag] =
              AbsExpr(reify(c.Expr[HF](tree).splice.apply[T](c.Expr[Arg1T](expr.tree).splice)))
            val arg1Tpe = tpe.baseType(typeOf[Poly1[Arg1] forSome {type Arg1[X]}].typeSymbol) match {
              case TypeRef(_, sym, List(arg1)) => arg1
            }
            val tTpe = expr.tpe match {
              case TypeRef(_, sym, List(t)) => t
              case t if t =:= appliedType(arg1Tpe, List(t)) => t
            }
            genApply(c.WeakTypeTag(tpe), c.WeakTypeTag(tTpe), c.WeakTypeTag(appliedType(arg1Tpe, List(tTpe))))
          }
        }
        object Poly1Expr {
          def apply[T: WeakTypeTag](expr: Expr[T]): Poly1Expr = new Poly1Expr(expr.tree, tpeFromExpr(expr))
          def apply(expr: AbsExpr): Poly1Expr = new Poly1Expr(expr.tree, expr.tpe)
        }
        
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
          def filter(t: Type): ListExpr
          def filterNot(t: Type): ListExpr
          def find(t: Type): AbsExpr
          def find(f: Type => Boolean): AbsExpr
          def apply(i: Expr[Int]): AbsExpr
          def length: Expr[Int]
          def indexOf(t: Type): Expr[Int]
          def take(i: Expr[Int]): ListExpr
          def takeRight(i: Expr[Int]): ListExpr
          def drop(i: Expr[Int]): ListExpr
          def dropRight(i: Expr[Int]): ListExpr
          def takeWhile(t: Type): ListExpr
          def dropWhile(t: Type): ListExpr
          def span(t: Type): (ListExpr, ListExpr)
          def splitAt(i: Expr[Int]): (ListExpr, ListExpr)
          def unzip: (ListExpr, ListExpr)
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
        }

        object ListExpr {
          def apply[T: WeakTypeTag](expr: Expr[T]): ListExpr = {
            val tpe = tpeFromExpr(expr)
            if(tpe <:< typeOf[HNil])
              HNilExpr
            else if(tpe <:< typeOf[_ :: _])
              HListExpr(expr.tree, tpe)
            else
              sys.error("Unknown HList type")
          }
          def apply(tree: Tree, tpe: Type): ListExpr = {
            if(tpe <:< typeOf[HNil])
              HNilExpr
            else if(tpe <:< typeOf[_ :: _])
              HListExpr(tree, tpe)
            else
              sys.error("Unknown HList type")
          }
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
             def genCons[E: WeakTypeTag, H: WeakTypeTag, T <: HList: WeakTypeTag]: ListExpr =
               ListExpr(reify(c.Expr[E](e.tree).splice :: c.Expr[H :: T](tree).splice))
             genCons(c.WeakTypeTag(e.tpe), c.WeakTypeTag(headTpe), c.WeakTypeTag(tailTpe))
          }

          def reverse: ListExpr = tail.reverse :+ head

          def last: AbsExpr = reverse.head

          def init: ListExpr = reverse.tail.reverse

          def :+(e: AbsExpr) = head :: (tail :+ e)

          def ++(l: ListExpr) = l match {
            case HNilExpr => this
            case hl @ HListExpr(_, _) => hl.tail ++ (hl.head :: l)
          }
          
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
          /*{
            val found = typeLookup(t, head.tpe)
            if(found != EmptyTree)
              head
            else
              tail.find(t)
          }*/

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
            if(!isLiteral(i.tree))
              sys.error("Only literals are allowed as HList indices")
            else if(i == reify(0))
              head
            else
              tail.apply(reify(i.splice - 1))
          }

          def indexOf(t: Type): Expr[Int] = {
            val found = c.inferImplicitValue(appliedType(typeOf[_ => _], List(head.tpe, t)))
            if(found != EmptyTree)
              reify(0)
            else
              reify(1 + tail.indexOf(t).splice)
          }

          def length: Expr[Int] = reify(1 + tail.length.splice)

          def take(i: Expr[Int]): ListExpr = {
            if(!isLiteral(i.tree))
              sys.error("Only literals are allowed as HList indices")
            if(i == reify(0))
              HNilExpr
            else
              head :: take(reify(i.splice - 1))
          }

          def takeRight(i: Expr[Int]): ListExpr = reverse.take(i).reverse 

          def drop(i: Expr[Int]): ListExpr = takeRight(reify(length.splice - i.splice))

          def dropRight(i: Expr[Int]): ListExpr = take(reify(length.splice - i.splice))

          def takeWhile(t: Type): ListExpr = {
            val found = c.inferImplicitView(head.tree, head.tpe, t)
            if(found != EmptyTree)
              head :: tail.takeWhile(t)
            else
              HNilExpr
          }

          def dropWhile(t: Type): ListExpr = {
            val found = c.inferImplicitValue(appliedType(typeOf[_ => _], List(head.tpe, t)))
            if(found != EmptyTree)
              this
            else
              tail.dropWhile(t)
          }

          def span(t: Type): (ListExpr, ListExpr) = (takeWhile(t), dropWhile(t))

          def splitAt(i: Expr[Int]): (ListExpr, ListExpr) = (take(i), drop(i))

          def unzip: (ListExpr, ListExpr) = {
            val remainder = tail.unzip
            (head.asInstanceOf[TupleExpr].first :: tail.unzip._1, 
             head.asInstanceOf[TupleExpr].second :: tail.unzip._2)
          }

          def updated(i: Expr[Int], e: AbsExpr): ListExpr = {
            if(!isLiteral(i.tree))
              sys.error("Only literals are allowed as HList indices")
            if(i == reify(0))
              e :: tail
            else
              head :: updated(reify(i.splice - 1), e)
          }

          def zip(l: ListExpr): ListExpr = {
            if(l == HNilExpr)
              HNilExpr
            else
              TupleExpr(head, l.head) :: tail.zip(l.tail)
          }

          def zipAll(l: ListExpr, e1: AbsExpr, e2: AbsExpr): ListExpr = ???

          protected def reverseIndexes: ListExpr = tail match {
            case HNilExpr => AbsExpr(reify(0)) :: HNilExpr
            case hltail @ HListExpr(_, _) => reify(length.splice - 1) :: hltail.reverseIndexes
          } 

          def zipWithIndex: ListExpr = zip(reverseIndexes.reverse)

          protected def tpes: List[Type] = tail match {
            case HNilExpr => List(head.tpe)
            case hltail @ HListExpr(_, _) => head.tpe :: hltail.tpes
          }

          def toList: AbsExpr = {
            def genList[A: WeakTypeTag]: AbsExpr =
              AbsExpr(reify(c.Expr[A](head.tree).splice :: c.Expr[List[A]](tail.toList.tree).splice))
            genList(c.WeakTypeTag(lub(tpes)))
          }

          def toArray: AbsExpr = ??? //AbsExpr(reify(toList.splice.toArray))

          def tupled: AbsExpr = ???

          def unify: ListExpr = ??? // reify(toList.splice).toHList

          def startsWith(l: ListExpr): Expr[Boolean] = ???
            //reify(toList.splice.startsWith(l.toList.splice))

          def endsWith(l: ListExpr): Expr[Boolean] = ???
            //reify(toList.splice.endsWith(l.toList.splice))

          /** find applicable function in hf HList for each element of HList
           *  e.g. for element of type X find Poly1[Arg1] such that
           *  X <:< Arg1[_]
           */
          def map(hf: ListExpr): ListExpr =
            Poly1Expr(hf.find((t: Type) => {
              val arg1Tpe = t.baseType(typeOf[Poly1[Arg1] forSome {type Arg1[X]}].typeSymbol) match {
                case TypeRef(_, sym, List(arg1)) => arg1
              }
              head.tpe <:< appliedType(arg1Tpe, List(WildcardType))
            })).apply(head) :: tail.map(hf)

          def flatten: ListExpr = {
            if(head.tpe <:< typeOf[HNil])
              tail.flatten
            else if(head.tpe <:< typeOf[_ :: _])
              (head :: HNilExpr) ++ tail.flatten
            else
              sys.error("Can not flatten HList containing elements of type " + head.tpe)
          }

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

          def foldLeft(e: AbsExpr)(f: ListExpr): AbsExpr = (e :: this).reduceLeft(f)

          def foldRight(e: AbsExpr)(f: ListExpr): AbsExpr = (this :+ e).reduceRight(f)

        }
        object HListExpr {
          def apply[T](expr: Expr[T]): HListExpr = new HListExpr(expr.tree, tpeFromExpr(expr))
          def apply(tree: Tree, tpe: Type): HListExpr = new HListExpr(tree, tpe)
          def unapply(hl: HListExpr): Option[(Tree, Type)] = Some((hl.tree, hl.tpe))
        }
        
        implicit def exprToHList[H, T <: HList](expr: Expr[H :: T]): HListExpr =
          HListExpr(expr.tree, tpeFromExpr(expr))
        /* 
        class ListOps[A](l: Expr[List[A]]) {
          def toHList: ListExpr =
            if(l == Nil)
              HNilExpr
            else
              AbsExpr(reify(l.splice.head)) :: reify(l.splice.tail).toHList 
        }
        implicit def mkListOps[A](l: Expr[List[A]]): ListOps[A] = new ListOps[A](l)
        */

        case object HNilExpr extends ListExpr(reify(HNil).tree, typeOf[HNil]) {
          def ::(e: AbsExpr): ListExpr = {
            def genCons[E: WeakTypeTag]: ListExpr =
              HListExpr(reify(c.Expr[E](e.tree).splice :: HNil))
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
          def find(t: Type): AbsExpr = sys.error("Element of type " + t + " not found")
          def find(f: Type => Boolean): AbsExpr = sys.error("Element not found")
          def filter(t: Type): ListExpr = HNilExpr
          def filterNot(t: Type): ListExpr = HNilExpr
          def apply(i: Expr[Int]): AbsExpr = sys.error("HNil has no element")
          def indexOf(t: Type): Expr[Int] = sys.error("HNil has no element")
          def length: Expr[Int] = reify(0)
          def take(i: Expr[Int]): ListExpr = HNilExpr
          def takeRight(i: Expr[Int]): ListExpr = HNilExpr
          def drop(i: Expr[Int]): ListExpr = HNilExpr
          def dropRight(i: Expr[Int]): ListExpr = HNilExpr
          def takeWhile(t: Type): ListExpr = HNilExpr
          def dropWhile(t: Type): ListExpr = HNilExpr
          def unzip: (ListExpr, ListExpr) = (HNilExpr, HNilExpr)
          def updated(i: Expr[Int], e: AbsExpr): ListExpr = sys.error("HNil can not be updated")
          def span(t: Type): (ListExpr, ListExpr) = (HNilExpr, HNilExpr)
          def splitAt(i: Expr[Int]): (ListExpr, ListExpr) = (HNilExpr, HNilExpr)
          def zip(l: ListExpr): ListExpr = HNilExpr
          def zipAll(l: ListExpr, e1: AbsExpr, e2: AbsExpr): ListExpr = ???
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
        }

      }

      def hListContext(c: Context) = new HListContext[c.type](c)

      /** Now mapping functions working on AbsExpr to macro implementations
       *  which are working on plain Expr
       */
      
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

      def find[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).find(c.weakTypeOf[E]).toExpr

      def filter[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).filter(c.weakTypeOf[E]).toExpr

      def filterNot[L <: HList: c.WeakTypeTag, E: c.WeakTypeTag](c: Context) =
        hListContext(c).ListExpr(c.Expr[L](c.prefix.tree)).filterNot(c.weakTypeOf[E]).toExpr

      def map[L <: HList: c.WeakTypeTag, HF <: HList: c.WeakTypeTag](c: Context)(hf: c.Expr[HF]) = {
        val hl = hListContext(c)
        (hl.ListExpr(c.Expr[L](c.prefix.tree)).map(hl.ListExpr(hf))).toExpr
      }


  }




