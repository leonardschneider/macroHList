package macroHList

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.mutable.Map

trait CachedContext extends RichContext {
  import c.universe._

  def cache[B: WeakTypeTag](body: Expr[B]): Expr[B] = {
  
    val DefDef(_, fun, _, argss, _, _) = c.enclosingMethod
    def mkArgsTuple(args: List[ValDef]) =
      TupleExpr(args.map{case ValDef(_, name, _, _) => AbsExpr(Ident(name), c.typeCheck(Ident(name)).tpe)}: _*)
    val argssTuple = TupleExpr(argss.map(args => mkArgsTuple(args).toAbsExpr): _*).toExpr
    //c.echo(NoPosition, "args types " + argss.map(_.map{case ValDef(_, name, tpt, _) => "" + name + ": " + c.typeCheck(Ident(name)).tpe}))
    //c.echo(NoPosition, ""+argssTuple)
    val key = reify((c.literal(fun.decoded).splice, argssTuple.splice))

    reify {
      macroHList.cached.defaultMap.getOrElse(key.splice, {
        val res = body.splice
        macroHList.cached.defaultMap += key.splice -> res
        res
      }).asInstanceOf[B]
    }

  }

}

object cached {
  var defaultMap: Map[Any, Any] = Map.empty

  def cachedContext(c0: Context) =
    new {
      val c: c0.type = c0
    } with CachedContext

  def apply[B](body: B): B = macro impl[B]

  def impl[B: c.WeakTypeTag](c: Context)(body: c.Expr[B]): c.Expr[B] = {
    import c.universe._

    cachedContext(c).cache(body)

  }

}

