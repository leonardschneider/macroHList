package macroHList.test


import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import macroHList._
import TypeOperators._

@RunWith(classOf[JUnitRunner])
class HlistTest extends SpecificationWithJUnit {

  val nehl = 1 :: "Hey" :: true :: HNil
  val identity = new Poly1[Id] { def apply[T](x: T) = x }
  val lift = new Poly1[Id] { def apply[T](x: T): Option[T] = Some(x) }
  val unlift = new Poly1[Option] { def apply[T](x: Option[T]) = x.get }
  val hlistify = new Poly1[Id] { def apply[T](x: T) = x :: HNil}

  "A non empty HList" should {
    "be non empty" in {
      nehl.isEmpty must beFalse
      nehl.nonEmpty must beTrue
    }
    "have last element equal to reverse.head" in {
      nehl.last must beEqualTo(nehl.reverse.head)
    }
    "be the concatenation of its head and tail" in {
      nehl must beEqualTo(nehl.head :: nehl.tail)
    }
    "be the concatenation of its init and last element" in {
      nehl must beEqualTo(nehl.init :+ nehl.last)
    }
    "be its own mapping through the identity function" in {
      nehl.map(identity :: HNil) must beEqualTo(nehl)
    }
    "be its own mapping through lift-unlift composition" in {
      nehl.map(lift :: HNil).map(unlift :: HNil) must beEqualTo(nehl)
    }
    "be itself with a blank filter" in {
      nehl.filter[Any] must beEqualTo(nehl)
      nehl.filterNot[Nothing] must beEqualTo(nehl)
    }
    "be HNil with a plain filter" in {
      nehl.filter[Nothing] must beEqualTo(HNil)
      nehl.filterNot[Any] must beEqualTo(HNil)
    }
    "contain its head" in {
      nehl.contains[nehl.Head] must beTrue
    }
    /*"contain its last element" in {
      val last = nehl.last
      nehl.contains[last.type] must beTrue
    }*/
    "be itself after being reversed twice" in {
      nehl.reverse.reverse must beEqualTo(nehl)
    }
    "be itself after HListifying and flattening" in {
      nehl.map(hlistify :: HNil).flatten must beEqualTo(nehl)
    }

  }

}

