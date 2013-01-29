macroHList
==========

A macro implementation of HList

**WORK IN PROGRESS**

# How to use

## HList operations for tuples

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

## Function Cached Macro

For (pure) function memoization, you can use the cached macro just before the function definition, as
show after.

```scala
Welcome to Scala version 2.10.0 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_37).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import macroHList._
import macroHList._

scala> import util._
import util._

scala> def fac(i: BigInt): BigInt = if(i<2) 1 else i*fac(i-1)
fac: (i: BigInt)BigInt

scala> def facCached(i: BigInt): BigInt = cached { if(i < 2) 1 else i * facCached(i - 1) }
facCached: (i: BigInt)BigInt

scala> time { (1 to 10000).foreach(_ => fac(200)) }
471409 microseconds

scala> time { (1 to 10000).foreach(_ => facCached(200)) }
14720 microseconds
```

