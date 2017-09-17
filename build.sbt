name := "tdrive"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.twitter" %% "chill" % "0.9.2"

libraryDependencies += "com.esotericsoftware" % "kryo" % "4.0.1"

logBuffered in Test := false

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"