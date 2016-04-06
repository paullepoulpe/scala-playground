
scalaVersion := "2.11.8"

name := "scala-playground"

scalacOptions ++= Seq("-feature", "-deprecation")

triggeredMessage in ThisBuild := Watched.clearWhenTriggered

maxErrors := 6
