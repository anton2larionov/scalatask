name := "scalatask"

version := "1.0"

scalaVersion := "2.11.7"

assemblyJarName in assembly := "scalatask.jar"
mainClass in assembly := Some("by.scalalab.Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"
    