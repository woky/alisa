import sbt._
import sbt.Keys._

object AlisaBuild extends Build {

	val scalaVer = "2.10.3"

	val projectInfo = Seq(
		name := "alisa",
		description <<= name(_.capitalize + " - Cute and kindhearted IRC bot"),
		version := "0.2"
	)

	def mkJavaDeps(group: String, version: String, mods: String*) =
		mods.map(group % _ % version)

	val nettyDeps = {
		val v = "4.0.0.CR6"
		val mods = Seq(
			"common",
			"buffer",
			"transport",
			"handler",
			"codec",
			"codec-http")
		mkJavaDeps("io.netty", v, mods.map("netty-" + _): _*)
	}

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
		"com.jsuereth" %% "scala-arm" % "1.3",
		"de.u-mass" % "lastfm-java" % "0.1.2",
		/*
		"org.scala-lang" % "scala-reflect" % scalaVer,
		"net.java.dev.jna" % "jna" % "3.4.0",
		"org.kohsuke" % "akuma" % "1.8",
		*/

		/*
		 * Test dependencies
		 */
		"junit" % "junit" % "4.11" % "test",
		"org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test",
		"org.powermock" % "powermock-module-junit4" % "1.5" % "test",
		"org.powermock" % "powermock-api-easymock"  % "1.5" % "test"
	) ++ nettyDeps

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
		) ++ OneJar.oneJarSettings
	)
}
