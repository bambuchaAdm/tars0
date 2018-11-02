name := "tars0"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies +=  "org.mockito" % "mockito-core" % "2.11.0" % "test"

scalacOptions ++= Seq("-deprecation", "-feature")
