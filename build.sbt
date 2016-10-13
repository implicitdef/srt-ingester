name := """srt-ingester"""

version := "1.0"

scalaVersion := "2.11.7"

resolvers += Resolver.jcenterRepo
libraryDependencies += "com.github.implicitdef" %% "toolbox" % "0.5.0"

libraryDependencies += "commons-io" % "commons-io" % "2.5"

libraryDependencies += "net.lingala.zip4j" % "zip4j" % "1.3.2"

libraryDependencies += "net.sourceforge.jchardet" % "jchardet" % "1.0"

libraryDependencies += "com.github.wtekiela" % "opensub4j" % "0.1.2"
