scalaVersion := "2.10.0-RC1"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )

libraryDependencies += "org.specs2" %% "specs2" % "1.13-SNAPSHOT" % "test"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

resolvers in ThisBuild ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
