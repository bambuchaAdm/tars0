name := "tars0"

version := "0.1"

scalaVersion := "2.12.7"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies +=  "org.mockito" % "mockito-core" % "2.11.0" % "test"
libraryDependencies += "com.softwaremill.macmemo" %% "macros" % "0.4"

scalacOptions ++= Seq("-deprecation", "-feature")
