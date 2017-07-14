name := "ScalaSoup"

organization := "top.wagzhi"

version := "1.0"

scalaVersion := "2.11.8"

publishTo := Some(Resolver.file("file",  new File( Path.userHome.absolutePath+"/.m2/repository")) )

libraryDependencies ++= Seq(
  "org.jsoup" % "jsoup" % "1.10.1" withSources(),
  "org.scalatest" %% "scalatest" % "3.0.1" % "test" exclude("org.scala-lang", "scala-reflect")
)