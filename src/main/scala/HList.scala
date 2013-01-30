package macroHList

import scala.language.experimental.macros

trait HList {
  type Self <: HList
  def length: Int
  def isEmpty: Boolean
  def nonEmpty = !isEmpty
  def ::[E](e: E): Any = macro HListContext.prepend[Self, E]
  def :+[E](e: E): Any = macro HListContext.append[Self, E]
  def last: Any = macro HListContext.last[Self]
  def reverse: Any = macro HListContext.reverse[Self]
  def init: Any = macro HListContext.init[Self]
  /** Concatenate two HLists together.
   */
  def ++[L2 <: HList](l2: L2): Any = macro HListContext.++[Self, L2]
  def updated[E](i: Int, e: E): Any = macro HListContext.updated[Self, E]
  /** Tell whether the HList contains an element of type E or not.
   */
  def containsType[E]: Boolean = macro HListContext.containsType[Self, E]
  def contains[E](e: E): Boolean = macro HListContext.contains[Self, E]
  def diff[L2 <: HList](l2: L2): Any = macro HListContext.diff[Self, L2]
  /** Find first element of type E in this HList
   */
  def findType[E]: Any = macro HListContext.findType[Self, E]
  def find[E](f: E => Boolean): Any = macro HListContext.find[Self, E]
  /** Filter the HList so only elements of type E remains.
   *  If E is an existential type, it is used as witness.
   *  Combined with TypeOperators, it enables to build complex filters,
   *  such as conjunctions, disjunctions, etc
   */
  def filter[E]: Any = macro HListContext.filter[Self, E]
  /** The complementary of filter
   */
  def filterNot[E]: Any = macro HListContext.filterNot[Self, E]
  /** Transform this HList by applying hf
   */
  //def map[HF <: HList](hf: HF) = macro HListContext.map[Self, HF]
  def map[HF](hf: HF) = macro HListContext.map[Self, HF]
  def flatMap[HF](hf: HF) = macro HListContext.flatMap[Self, HF]
  /** Flatten an HList of HLists to an HListContext.
   */
  def flatten = macro HListContext.flatten[Self]
  /** Get the i-th element of this HListContext. Only compile time known index is allowed.
   */
  def apply(i: Int): Any = macro HListContext.getIndex[Self]
  /** Returns the index of the first element of type E in this HListContext.
   */
  def indexOfType[E]: Int = macro HListContext.indexOfType[Self, E]
  def indexOfType[E](from: Int): Int = macro HListContext.indexOfTypeFrom[Self, E]
  def indexOf[E](e: E): Int = macro HListContext.indexOf[Self, E]
  def indexOf[E](e: E, from: Int): Int = macro HListContext.indexOfFrom[Self, E]
  /** Returns the index of the last element of type E in this HListContext.
   */
  def lastIndexOf[E]: Int = macro HListContext.lastIndexOf[Self, E]
  def lastIndexOf[E](end: Int): Int = macro HListContext.lastIndexOfEnd[Self, E]
  /** Take the first i elements of this HListContext. Only compile time known number is allowed
   *  as argument.
   */
  def take(i: Int): Any = macro HListContext.take[Self]
  def takeRight(i: Int): Any = macro HListContext.takeRight[Self]
  def drop(i: Int): Any = macro HListContext.drop[Self]
  def dropRight(i: Int): Any = macro HListContext.dropRight[Self]
  def takeWhile[E]: Any = macro HListContext.takeWhile[Self, E]
  def dropWhile[E]: Any = macro HListContext.dropWhile[Self, E]
  def span[E]: Any = macro HListContext.span[Self, E]
  def splitAt(i: Int): Any = macro HListContext.splitAt[Self]
  /** Unzip an HList of tuples to a tuple of HLists. Does not compile if the HList
   *  does not only contains tuples.
   */
  def unzip: Any = macro HListContext.unzip[Self]
  /** Zip two HLists to an HList of tuples.
   */
  def zip[L2 <: HList](l2: L2): Any = macro HListContext.zip[Self, L2]
  def zipAll[L2 <: HList, E1, E2](l2: L2, e1: E2, e2: E2): Any = macro HListContext.zipAll[Self, L2, E1, E2]
  def zipWithIndex: Any = macro HListContext.zipWithIndex[Self]
  /** Transform this HList to a standard List of the least upper bound type of the HList elements.
   */
  def toList: Any = macro HListContext.toList[Self]
  /** Transform this HList to a standard Array of the least upper bound type of the HList elements.
   */
  def toArray: Any = macro HListContext.toArray[Self]
  def startsWith[L2 <: HList](l2: L2): Boolean = macro HListContext.startsWith[Self, L2]
  def endsWith[L2 <: HList](l2: L2): Boolean = macro HListContext.endsWith[Self, L2]
  def count[HF](hf: HF): Int = macro HListContext.count[Self, HF]
  def mkString(start: String, sep: String, end: String): String
  def mkString(sep: String): String
  def mkString: String
  def toTuple: Any = macro HListContext.toTuple[Self]
  def toClass: Any = macro HListContext.toClass[Self]
  def reduceLeft[F](f: F): Any = macro HListContext.reduceLeft[Self, F]
  def reduceRight[F](f: F): Any = macro HListContext.reduceRight[Self, F]
  def foldLeft[T, F](t: T)(f: F): Any = macro HListContext.foldLeft[Self, T, F]
  def foldRight[T, F](t: T)(f: F): Any = macro HListContext.foldRight[Self, T, F]
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

case object HNil extends HNil

object HList {

  def fromTuple[T](tup: T) = macro HListContext.fromTupleImpl[T]

  def fromClass[C, U](clazz: C, unapply: U) = macro HListContext.fromClassImpl[C, U]

  def fromClass[C](clazz: C) = macro HListContext.fromClassDirectImpl[C]

  def fromTraversable[T <: Traversable[_]](list: T) = macro HListContext.fromTraversableImpl[T]

}
