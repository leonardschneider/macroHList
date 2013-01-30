package macroHList

import scala.language.implicitConversions

import scala.reflect.macros.Context
import scala.reflect.macros.Macro

trait RichContext extends Macro {

  //val c: Context
  import c.universe._

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

  case class TupleExpr(exprs: AbsExpr*) {
    def length = exprs.length
    def trees = exprs.map(_.tree).toList
    def tpes = exprs.map(_.tpe).toList
    def apply(i: Int) = exprs(i - 1)
    def toAbsExpr = {
      val tupSym = rootMirror.staticModule("scala.Tuple" + length)
      val tupTree = treeBuild.mkMethodCall(tupSym, newTermName("apply"), tpes, trees)
      AbsExpr(tupTree, c.typeCheck(tupTree).tpe)
    }
    def toExpr = toAbsExpr.toExpr
  }
  object TupleExpr {
    def fromTuple(tup: AbsExpr): TupleExpr = {
      // get the tuple symbol tupleX
      val tupSymOption = tup.tpe.baseClasses.find(_.fullName.matches("scala.Tuple[0-9]+"))
      if(!tupSymOption.isDefined)
        c.error(NoPosition, "" + tup.tpe + " is not a tuple")
      val tupSym = tupSymOption.get
      // get the tuple arity
      val tupArity = tupSym.fullName.drop("scala.Tuple".length).toInt
      // get tuple element trees
      val tupTrees = (1 to tupArity).map(i =>
        treeBuild.mkAttributedSelect(tup.tree, tup.tpe.member(newTermName("_" + i))))
      // get tuple element types
      val tupTpes = tup.tpe match {
        case TypeRef(_, _, tpes) => tpes
      }
      new TupleExpr((tupTrees zip tupTpes).map{case (tree, tpe) => AbsExpr(tree, tpe)}: _*)
    }
  }
  implicit def tupleExprToAbsExpr(t: TupleExpr): AbsExpr = t.toAbsExpr


/*
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
  */

  class PolyExpr(tree: Tree, tpe: Type) extends AbsExpr(tree, tpe) {
    def apply(exprs: List[AbsExpr]): AbsExpr = {
      //val tree = c.typeCheck(Apply(tpe.member(newTermName("apply")), exprs.map(_.tree).toSeq: _*))
      // Dunno why I have to do this, but else it crashes when passing lambda directly as arguments of map
      /*val applySym = tpe.member(newTermName("apply")).asMethod
      val argSyms = applySym.paramss.flatten
      val funTree = Function(argSyms.map(ValDef(_)), treeBuild.mkMethodCall(applySym, argSyms.map(s =>
          treeBuild.mkAttributedIdent(s)
      )))
      c.echo(NoPosition, "funTree:\n" + funTree)*/
      //val resTree = Apply(tree, exprs.map(_.tree))
      val resTree = Apply(tree, exprs.map(_.tree))
      //val resTree = treeBuild.mkMethodCall(c.typeCheck(tree).symbol, exprs.map(_.tree))
      val resTpe = c.typeCheck(Apply(tree, exprs.map(_.tree))).tpe
      c.echo(NoPosition, "resTree:\n" + resTree)
      c.echo(NoPosition, "resTpe: " + resTpe)
      AbsExpr(resTree, resTpe)
      
    }
    def reverse: PolyExpr = new PolyExpr(tree, tpe) {
      override def apply(exprs: List[AbsExpr]): AbsExpr = super.apply(exprs.reverse)
    }
  }
  object PolyExpr {
    def apply(e: AbsExpr) = {
      val res = e match {
        case e: PolyExpr => e
        case _ => new PolyExpr(e.tree, e.tpe)
      }
      c.echo(NoPosition, "PolyExpr tpe: " + res.tpe)
      c.echo(NoPosition, "PolyExpr tree:\n" + res.tree)
      c.echo(NoPosition, "PolyExpr apply tpe: " + res.tpe.member(newTermName("apply")).asMethod.typeSignature)
      res
    }

  }
}


