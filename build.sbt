


lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
  maxErrors := 6
)
lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

lazy val scalaCompiler = Def.setting {
  "org.scala-lang" % "scala-compiler" % "2.11.8"
}

lazy val core = (project in file(".")).
  dependsOn(macros).
  settings(commonSettings: _*).
  settings(
    name := "scala-playground"
  )

lazy val macros = (project in file("macros")).
  settings(commonSettings: _*).
  settings(
    name := "macro-playground",
    libraryDependencies += scalaReflect.value,
    libraryDependencies += scalaCompiler.value
  )






