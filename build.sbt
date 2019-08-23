name := "alisa"
description := name.value.capitalize + " - Cute and kindhearted IRC bot"
version := "0.3"
scalaVersion := "2.12.9"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.2.1",
  "pircbot" % "pircbot" % "1.5.0",
  "nu.validator.htmlparser" % "htmlparser" % "1.4",
  "com.ibm.icu" % "icu4j" % "53.1",
  "net.sf.jopt-simple" % "jopt-simple" % "4.6",
  "com.google.guava" % "guava" % "17.0",
  "com.jsuereth" % "scala-arm_2.12" % "2.0",
  "org.glassfish" % "javax.json" % "1.0.4",
  "junit" % "junit" % "4.11" % "test",
) ++ Seq(
  "lucene-analyzers-common",
  "lucene-core",
  "lucene-queryparser",
).map("org.apache.lucene" % _ % "4.7.2") ++ Seq(
  "netty-buffer",
  "netty-codec",
  "netty-codec-http",
  "netty-common",
  "netty-handler",
  "netty-transport",
).map("io.netty" % _ % "4.0.21.Final") ++ Seq(
  "powermock-api-easymock",
  "powermock-module-junit4",
).map("org.powermock" % _ % "1.5" % "test") ++ Seq(
  "com.google.api-client" % "google-api-client" % "1.19.1",
  "com.google.apis" % "google-api-services-customsearch" % "v1-rev46-1.20.0",
).map(_.exclude("com.google.guava", "guava-jdk5"))

Compile / scalaSource       := baseDirectory.value / "src"
Compile / javaSource        := baseDirectory.value / "src"
Compile / resourceDirectory := baseDirectory.value / "src"
Test    / scalaSource       := baseDirectory.value / "test"
Test    / javaSource        := baseDirectory.value / "test"
Test    / resourceDirectory := baseDirectory.value / "test"

exportJars := true
Compile / packageBin / mainClass := Some("alisa.Alisa")

lazy val alisaProject = (project in file(".")).settings(OneJar.oneJarSettings)
