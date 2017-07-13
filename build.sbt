name := "ScalaSoup"

organization := "top.wagzhi"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.10.1" withSources(),
  "org.scalatest" %% "scalatest" % "3.0.1" % "test" exclude("org.scala-lang", "scala-reflect")
)