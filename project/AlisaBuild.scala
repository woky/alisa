import sbt._
import sbt.Keys._

object AlisaBuild extends Build {

	val scalaVer = "2.10.2"

	val projectInfo = Seq(
		name := "alisa",
		description <<= name(_.capitalize + " - Cute and kindhearted IRC bot"),
		version := "0.2"
	)

	val deps = Seq(
		"com.typesafe" % "config" % "1.0.1",
		"pircbot" % "pircbot" % "1.5.0",
		"nu.validator.htmlparser" % "htmlparser" % "1.4",
		"com.ibm.icu" % "icu4j" % "51.2",
		"net.sf.jopt-simple" % "jopt-simple" % "4.5",
		"org.apache.lucene" % "lucene-core" % "4.3.0",
		"org.apache.lucene" % "lucene-analyzers-common" % "4.3.0",
		"org.apache.lucene" % "lucene-queryparser" % "4.3.0",
		"com.google.guava" % "guava" % "14.0.1",
		"com.google.code.findbugs" % "jsr305" % "2.0.1" % "provided",
		/*
		"org.scala-lang" % "scala-reflect" % scalaVer,
		"net.java.dev.jna" % "jna" % "3.4.0",
		"org.kohsuke" % "akuma" % "1.8",
		*/

		/*
		 * Test dependencies
		 */
		"junit" % "junit" % "4.11" % "test"
	)

	lazy val proj = Project(
		"alisa",
		file("."),
		settings = Defaults.defaultSettings ++ projectInfo ++ Seq(
			scalaVersion := scalaVer,
			scalaSource in Compile <<= baseDirectory / "src",
			javaSource in Compile <<= baseDirectory / "src",
			resourceDirectory in Compile <<= baseDirectory / "resources",
			libraryDependencies ++= deps,
			exportJars := true,
			mainClass in(Compile, packageBin) := Some("alisa.Alisa")
		) ++ OneJar.oneJarSettings ++ net.virtualvoid.sbt.graph.Plugin.graphSettings
	)
}
