package macroHList.test


import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import macroHList._
import TypeOperators._

@RunWith(classOf[JUnitRunner])
class TypeOperatorsTest extends SpecificationWithJUnit {

  def exists[T](implicit e: T = null) = if(e == null) false else true

  "Type operators" should {
    "detect a conjunction of implicits" in {
      {
        implicit val x = 42
        exists[Int |:| String] must beFalse
      }
      {
        implicit val s = "Hello World"
        exists[Int |:| String] must beFalse
      }
      {
        implicit val x = 42
        implicit val s = "Hello World"
        exists[Int &:& String] must beTrue
      }
      {
        exists[Int |:| String] must beFalse
      }
    }

    "detect a disjunction of implicits" in {
      {
        implicit val x = 42
        exists[Int |:| String] must beTrue
      }
      {
        implicit val s = "Hello World"
        exists[Int |:| String] must beTrue
      }
      {
        implicit val x = 42
        implicit val s = "Hello World"
        exists[Int |:| String] must beTrue
      }
      {
        exists[Int |:| String] must beFalse
      }
    }.pendingUntilFixed("SI-5923")
  
  }

}

