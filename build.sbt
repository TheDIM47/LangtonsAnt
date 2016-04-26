name := "LangtonsAnt"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

val monocleVersion = "1.2.1"

libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %% "monocle-core" % monocleVersion
  , "org.scalatest" %% "scalatest" % "2.2.6" % Test
)

