package macroHList

import scala.language.existentials
import scala.language.higherKinds

trait Poly1[Arg1[_]] {
  def apply[T](x: Arg1[T]): (R forSome {type R})
}

