name := "relatedtexts"

version := "1.0"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "org.jsoup" % "jsoup" % "1.7.3",
    "com.typesafe.play" % "play-json_2.10" % "2.2.2"
)

scalaVersion := "2.10.3" 