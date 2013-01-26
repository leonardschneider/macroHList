macroHList
==========

A macro implementation of HList

WORK IN PROGRESS

* How to use

HLists can be thought as a generalized tuple, i.e. not limited in size. In addition it offers list like operations, such as append, prepend, map, etc.

Here is an example session.

```scala
Welcome to Scala version 2.10.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_37).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import macroHList._
import macroHList._

scala> val l1 = 1 :: "two" :: 3.0 :: HNil
l1: macroHList.::[Int,macroHList.::[String,macroHList.::[Double,macroHList.HNil.type]]] = HList(1, two, 3.0)

scala> l1.reverse
res0: macroHList.::[Double,macroHList.::[String,macroHList.::[Int,macroHList.HNil.type]]] = HList(3.0, two, 1)

scala> l1.toTuple
res1: (Int, String, Double) = (1,two,3.0)

scala> l1 ++ (true :: HNil)
res2: macroHList.::[Int,macroHList.::[String,macroHList.::[Double,macroHList.::[Boolean,macroHList.HNil.type]]]] = HList(1, two, 3.0, true)

scala> import macroHList.Implicits._
import macroHList.Implicits._

scala> (1, "two", 3.0).reverse.toTuple
res4: (Double, String, Int) = (3.0,two,1)
```

