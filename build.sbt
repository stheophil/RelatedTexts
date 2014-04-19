name := "relatedtexts"

version := "1.0"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "org.jsoup" % "jsoup" % "1.7.3",
    "com.typesafe.play" % "play-json_2.10" % "2.2.2",
    "org.specs2" %% "specs2" % "2.3.11" % "test"
)

scalaVersion := "2.10.3" 