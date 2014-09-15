import AssemblyKeys._

name := "crossword"

version := "1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

assemblySettings

jarName in assembly := "CrossWord.jar"

mainClass in assembly := Some("org.egor.crossword.model.Main")