package macroHList

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.macros.Context
import scala.reflect.ClassTag

import TypeOperators._
import Poly._


trait HListContext extends RichContext {
  import c.universe._

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
    //def map(hf: ListExpr): ListExpr
    def map(hf: AbsExpr): ListExpr
    def flatten: ListExpr
    def flatMap(hf: AbsExpr): ListExpr
    def foldLeft(e: AbsExpr)(l: AbsExpr): AbsExpr
    def foldRight(e: AbsExpr)(l: AbsExpr): AbsExpr
    def reduceLeft(l: AbsExpr): AbsExpr
    def reduceRight(l: AbsExpr): AbsExpr
    def reduce(f: AbsExpr): AbsExpr = {
      val res = treeBuild.mkMethodCall(f.tree, trees)
      AbsExpr(c.Expr(res))
    }
    def count(hf: AbsExpr): Expr[Int]
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
      TupleExpr(headTup(1) :: ListExpr(tailTup(1)), 
                headTup(2) :: ListExpr(tailTup(2)))
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

    def map(hf: AbsExpr): ListExpr = PolyExpr(hf).apply(List(head)) :: tail.map(hf)

    def count(hf: AbsExpr): Expr[Int] =
      reify(
        {
          if(c.Expr[Boolean](PolyExpr(hf).apply(List(head)).tree).splice)
            1
          else
            0
        } + tail.count(hf).splice
      )
      //reify(c.Expr[List[Boolean]](map(hf).toList.tree).splice.map(b => if(b) 1 else 0).reduceLeft(_ + _))

    def flatten: ListExpr = ListExpr(head.tree, head.tpe) ++ tail.flatten

    def flatMap(hf: AbsExpr): ListExpr = map(hf).flatten

    def reduceLeft(hf: AbsExpr): AbsExpr =
      if(tail == HNilExpr)
        head
      else
        (PolyExpr(hf).apply(List(head, tail.head)) :: tail.tail).reduceLeft(hf)

    def reduceRight(hf: AbsExpr): AbsExpr = reverse.reduceLeft(PolyExpr(hf).reverse)

    def foldLeft(e: AbsExpr)(f: AbsExpr): AbsExpr = (e :: this).reduceLeft(f)

    def foldRight(e: AbsExpr)(f: AbsExpr): AbsExpr = (this :+ e).reduceRight(f)

    def trees: List[Tree] = head.tree :: tail.trees

    def tpes: List[Type] = head.tpe :: tail.tpes

    def toTuple: AbsExpr = {
      val length = c.eval(c.Expr[Int](this.length.tree))
      // Get tuple symbol we're interested in
      val tupSym = rootMirror.staticModule("scala.Tuple" + length)
      val tupTree = treeBuild.mkMethodCall(tupSym, TermName("apply"), tpes, trees)
      AbsExpr(c.Expr(tupTree))
    }

    /** Recursive class building based on companion object apply method
    *  which is supposed to be stored at the HList head, while the
    *  arguments constitute the tail.
    */
    def toClass: AbsExpr = {
      val applySym = head.tpe.member(TermName("apply")).asMethod
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
    //def map(hf: ListExpr): ListExpr = HNilExpr
    def map(hf: AbsExpr): ListExpr = HNilExpr
    def flatten: ListExpr = HNilExpr
    def flatMap(hf: AbsExpr): ListExpr = HNilExpr
    def foldLeft(e: AbsExpr)(f: AbsExpr): AbsExpr = e
    def foldRight(e: AbsExpr)(f: AbsExpr): AbsExpr = e
    def reduceLeft(f: AbsExpr): AbsExpr = sys.error("HNil can not be reduced")
    def reduceRight(f: AbsExpr): AbsExpr = sys.error("HNil can not be reduced")
    def count(hf: AbsExpr): Expr[Int] = reify(0)
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
      treeBuild.mkAttributedSelect(tup.tree, tup.tpe.member(TermName("_" + i))))
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
    val unapplyMethod = companionTpe.member(TermName("unapply"))
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

  def prepend[L <: HList: WeakTypeTag, E: WeakTypeTag](e: Expr[E]): Expr[Any] = {
    (AbsExpr(e) :: ListExpr(c.Expr[L](c.prefix.tree))).toExpr     
  }

  def append[L <: HList: WeakTypeTag, E: WeakTypeTag](e: Expr[E]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)) :+ AbsExpr(e)).toExpr
  }

  def reverse[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).reverse.toExpr

  def last[L <: HList: WeakTypeTag]: Expr[Any] =
    ListExpr(c.Expr[L](c.prefix.tree)).last.toExpr

  def init[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).init.toExpr

  def ++[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag](l2: Expr[L2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).++(ListExpr(l2))).toExpr
  }

  def updated[L <: HList: WeakTypeTag, E: WeakTypeTag](i: Expr[Int], e: Expr[E]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).updated(i, AbsExpr(e))).toExpr
  }

  def containsType[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).contains(c.weakTypeOf[E])

  def contains[L <: HList: WeakTypeTag, E: WeakTypeTag](e: Expr[E]) = {
    ListExpr(c.Expr[L](c.prefix.tree)).contains(AbsExpr(e))
  }

  def diff[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag](l2: Expr[L2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).diff(ListExpr(l2))).toExpr     
  }

  def findType[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).find(c.weakTypeOf[E]).toExpr

  def find[L <: HList: WeakTypeTag, E: WeakTypeTag](f: Expr[E => Boolean]) = {
    ListExpr(c.Expr[L](c.prefix.tree)).find(AbsExpr(f)).toExpr
  }

  def filter[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).filter(c.weakTypeOf[E]).toExpr

  def filterNot[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).filterNot(c.weakTypeOf[E]).toExpr

  def map[L <: HList: WeakTypeTag, HF: WeakTypeTag](hf: Expr[HF]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).map(AbsExpr(hf))).toExpr
  }

  def flatMap[L <: HList: WeakTypeTag, HF: WeakTypeTag](hf: Expr[HF]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).flatMap(AbsExpr(hf))).toExpr
  }

  def flatten[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).flatten.toExpr

  def getIndex[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).apply(i).toExpr

  def indexOfType[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).indexOf(c.weakTypeOf[E])

  def indexOfTypeFrom[L <: HList: WeakTypeTag, E: WeakTypeTag](from: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).indexOf(c.weakTypeOf[E], from)

  def indexOf[L <: HList: WeakTypeTag, E: WeakTypeTag](e: Expr[E]) = {
    ListExpr(c.Expr[L](c.prefix.tree)).indexOf(AbsExpr(e))
  }

  def indexOfFrom[L <: HList: WeakTypeTag, E: WeakTypeTag](e: Expr[E], from: Expr[Int]) = {
    ListExpr(c.Expr[L](c.prefix.tree)).indexOf(AbsExpr(e), from)
  }

  def lastIndexOf[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).lastIndexOf(c.weakTypeOf[E])

  def lastIndexOfEnd[L <: HList: WeakTypeTag, E: WeakTypeTag](end: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).lastIndexOf(c.weakTypeOf[E], end)

  def take[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).take(i).toExpr

  def takeRight[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).takeRight(i).toExpr

  def drop[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).drop(i).toExpr

  def dropRight[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).dropRight(i).toExpr

  def takeWhile[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).takeWhile(c.weakTypeOf[E]).toExpr

  def dropWhile[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).dropWhile(c.weakTypeOf[E]).toExpr

  def span[L <: HList: WeakTypeTag, E: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).span(c.weakTypeOf[E]).toExpr

  def splitAt[L <: HList: WeakTypeTag](i: Expr[Int]) =
    ListExpr(c.Expr[L](c.prefix.tree)).splitAt(i).toExpr

  def unzip[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).unzip.toExpr

  def zip[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag](l2: Expr[L2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).zip(ListExpr(l2))).toExpr     
  }

  def zipAll[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag,
             E1: WeakTypeTag, E2: WeakTypeTag](
             l2: Expr[L2], e1: Expr[E1], e2: Expr[E2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).zipAll(ListExpr(l2), AbsExpr(e1), AbsExpr(e2))).toExpr     
  }

  def zipWithIndex[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).zipWithIndex.toExpr

  def toList[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).toList.toExpr

  def toArray[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).toArray.toExpr

  def toTuple[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).toTuple.toExpr

  def toClass[L <: HList: WeakTypeTag] =
    ListExpr(c.Expr[L](c.prefix.tree)).toClass.toExpr

  def startsWith[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag](l2: Expr[L2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).startsWith(ListExpr(l2)))   
  }

  def endsWith[L <: HList: WeakTypeTag, L2 <: HList: WeakTypeTag](l2: Expr[L2]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).endsWith(ListExpr(l2)))   
  }

  def count[L <: HList: WeakTypeTag, HF: WeakTypeTag](hf: Expr[HF]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).count(AbsExpr(hf)))   
  }

  def reduceLeft[L <: HList: WeakTypeTag, F: WeakTypeTag](f: Expr[F]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).reduceLeft(AbsExpr(f))).toExpr 
  }

  def reduceRight[L <: HList: WeakTypeTag, F: WeakTypeTag](f: Expr[F]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).reduceRight(AbsExpr(f))).toExpr 
  }

  def foldLeft[L <: HList: WeakTypeTag, T: WeakTypeTag, F: WeakTypeTag](
    t: Expr[T])(f: Expr[F]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).foldLeft(AbsExpr(t))(AbsExpr(f))).toExpr 
  }

  def foldRight[L <: HList: WeakTypeTag, T: WeakTypeTag, F: WeakTypeTag](
    t: Expr[T])(f: Expr[F]) = {
    (ListExpr(c.Expr[L](c.prefix.tree)).foldRight(AbsExpr(t))(AbsExpr(f))).toExpr 
  }

  def fromTupleImpl[T: WeakTypeTag](tup: Expr[T]) = {
    fromTuple(AbsExpr(tup)).toExpr
  }

  def fromClassImpl[C: WeakTypeTag, U: WeakTypeTag](clazz: Expr[C],
    unapply: Expr[U]) = {
    fromClass(AbsExpr(clazz), AbsExpr(unapply)).toExpr 
  }

  def fromClassDirectImpl[C: WeakTypeTag](clazz: Expr[C]) = {
    fromClass(AbsExpr(clazz)).get.toExpr
  }

  def fromTraversableImpl[T <: Traversable[_]: WeakTypeTag](list: Expr[T]): Expr[Any] = {
    fromTraversable(AbsExpr(list)).toExpr
  }

}

