import sbt._
import sbt.Keys._

object AlisaBuild extends Build {

	val scalaVer = "2.10.0"

	val projectInfo = Seq(
		name := "alisa",
		description <<= name(_.capitalize + " - Cute and kindhearted IRC bot"),
		version := "0.1"
	)

	val deps = Seq(
		"pircbot" % "pircbot" % "1.5.0",
		"org.apache.httpcomponents" % "httpclient" % "4.2.1",
		"nu.validator.htmlparser" % "htmlparser" % "1.4",
		"com.ibm.icu" % "icu4j" % "49.1",
		"org.slf4j" % "slf4j-api" % "1.7.2",
		"org.slf4j" % "jcl-over-slf4j" % "1.7.2",
		"org.slf4j" % "log4j-over-slf4j" % "1.7.2",
		"org.slf4j" % "jul-to-slf4j" % "1.7.2",
		"ch.qos.logback" % "logback-classic" % "1.0.9",
		"net.sf.jopt-simple" % "jopt-simple" % "4.3",
		"org.apache.lucene" % "lucene-core" % "4.0.0",
		"org.apache.lucene" % "lucene-analyzers-common" % "4.0.0",
		"org.apache.lucene" % "lucene-queryparser" % "4.0.0",
		"com.google.guava" % "guava" % "13.0.1",
		"com.google.code.findbugs" % "jsr305" % "2.0.1",
		"com.google.inject" % "guice" % "3.0",
		"com.google.inject.extensions" % "guice-multibindings" % "3.0" excludeAll (
				ExclusionRule(organization = "com.google.inject", name = "guice", configurations = Seq("tests"))),
		"com.sun.jersey" % "jersey-core" % "1.15",
		"com.sun.jersey" % "jersey-server" % "1.15",
		"com.sun.jersey" % "jersey-servlet" % "1.15",
		"com.fasterxml.jackson.jaxrs" % "jackson-jaxrs-json-provider" % "2.1.1",
		"com.sun.jersey.contribs" % "jersey-guice" % "1.15",
		"org.eclipse.jetty" % "jetty-server" % "9.0.0.M3",
		"org.eclipse.jetty" % "jetty-servlet" % "9.0.0.M3",
		"org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016"
				artifacts (Artifact("javax.servlet", "jar", "jar")),
		/*
		"net.java.dev.jna" % "jna" % "3.4.0",
		"org.kohsuke" % "akuma" % "1.8",
		*/
		"org.scala-lang" % "scala-compiler" % scalaVer
	)

	val ivyExcl =
		<dependencies>
			<exclude org="commons-logging"/>
		</dependencies>

	lazy val proj = Project(
		"alisa",
		file("."),
		settings = Defaults.defaultSettings ++ projectInfo ++ Seq(
			scalaVersion := scalaVer,
			scalaSource in Compile <<= baseDirectory / "src",
			javaSource in Compile <<= baseDirectory / "src",
			resourceDirectory in Compile <<= baseDirectory / "resources",
			libraryDependencies ++= deps,
			ivyXML := ivyExcl,
			exportJars := true,
			mainClass in(Compile, packageBin) := Some("alisa.Alisa")
		) ++ OneJar.oneJarSettings ++ net.virtualvoid.sbt.graph.Plugin.graphSettings
	)
}
