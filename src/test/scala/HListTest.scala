package macroHList.test


import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import macroHList._

@RunWith(classOf[JUnitRunner])
class HlistTest extends SpecificationWithJUnit {

  val nehl = 1 :: "Hey" :: true :: HNil

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
      false must beTrue
    }
    "be itself with a blank filter" in {
      nehl.filter[Any] must beEqualTo(nehl)
      nehl.filterNot[Nothing] must beEqualTo(nehl)
    }
    "be HNil with a plain filter" in {
      nehl.filter[Nothing] must beEqualTo(HNil)
      nehl.filterNot[Any] must beEqualTo(HNil)
    }

  }

}

