scalaVersion in ThisBuild := "2.10.0"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature")

//scalacOptions ++= Seq("-Xlog-free-terms", "-explaintypes")

//libraryDependencies in ThisBuild += "org.specs2" %% "specs2" % "1.13" % "test"

libraryDependencies in ThisBuild <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

resolvers in ThisBuild ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
