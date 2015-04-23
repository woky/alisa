import sbt.Keys._
import sbt._

object AlisaBuild extends Build {

	val scalaVer = "2.11.6"

	val projectInfo = Seq(
		name := "alisa",
		description <<= name(_.capitalize + " - Cute and kindhearted IRC bot"),
		version := "0.2"
	)

	def mkJavaDeps(group: String, version: String, mods: String*) =
		mods.map(group % _ % version)

	val nettyDeps = {
		val v = "4.0.21.Final"
		val mods = Seq(
			"common",
			"buffer",
			"transport",
			"handler",
			"codec",
			"codec-http")
		mkJavaDeps("io.netty", v, mods.map("netty-" + _): _*)
	}

	val deps = (Seq(
		"com.typesafe" % "config" % "1.2.1",
		"pircbot" % "pircbot" % "1.5.0",
		"nu.validator.htmlparser" % "htmlparser" % "1.4",
		"com.ibm.icu" % "icu4j" % "53.1",
		"net.sf.jopt-simple" % "jopt-simple" % "4.6",
		"org.apache.lucene" % "lucene-core" % "4.7.2",
		"org.apache.lucene" % "lucene-analyzers-common" % "4.7.2",
		"org.apache.lucene" % "lucene-queryparser" % "4.7.2",
		"com.google.guava" % "guava" % "17.0",
		"com.jsuereth" %% "scala-arm" % "1.4",
		"org.glassfish" % "javax.json" % "1.0.4",
		"com.google.api-client" % "google-api-client" % "1.19.1",
		"com.google.apis" % "google-api-services-customsearch" % "v1-rev46-1.20.0",
		/*
		"org.scala-lang" % "scala-reflect" % scalaVer,
		"net.java.dev.jna" % "jna" % "3.4.0",
		"org.kohsuke" % "akuma" % "1.8",
		*/

		/*
		 * Test dependencies
		 */
		"junit" % "junit" % "4.11" % "test",
		"org.scalamock" %% "scalamock-scalatest-support" % "3.1.1" % "test",
		"org.powermock" % "powermock-module-junit4" % "1.5" % "test",
		"org.powermock" % "powermock-api-easymock"  % "1.5" % "test"
	) ++ nettyDeps).map(_.
			exclude("com.google.guava", "guava-jdk5")
			exclude("com.google.code.findbugs", "jsr305"))

	lazy val proj = Project(
		"alisa",
		file("."),
		settings = Defaults.defaultSettings ++ projectInfo ++ Seq(
			scalaVersion := scalaVer,
			scalaSource in Compile <<= baseDirectory / "src",
			javaSource in Compile <<= baseDirectory / "src",
			resourceDirectory in Compile <<= baseDirectory / "resources",
			scalaSource in Test <<= baseDirectory / "test",
			javaSource in Test <<= baseDirectory / "test",
			libraryDependencies ++= deps,
			exportJars := true,
			mainClass in(Compile, packageBin) := Some("alisa.Alisa")
		) ++ OneJar.oneJarSettings
	)
}
