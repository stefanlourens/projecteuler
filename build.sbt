scalaVersion := "2.11.0"

organization := "org.sl"

name := "projecteuler"

version := "1.0-SNAPSHOT"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

mainClass in (Compile, run) := Some("problems.Problem_0026")
