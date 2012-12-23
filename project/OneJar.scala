import java.io.{OutputStreamWriter, BufferedOutputStream}
import java.nio.file.{Path, Files}
import java.util.jar.{JarInputStream, JarEntry, JarOutputStream}
import java.util.regex.Pattern
import sbt._
import sbt.Keys._

/**
 * This has nothing to do with OneJar™.
 */
object OneJar {

	val oneJarTask = TaskKey[File]("onejar")
	val oneJarOutput = SettingKey[Path]("onejar-output")
	val oneJarFilters = SettingKey[Seq[String => Boolean]]("onejar-filters")

	val defaultFilters = Seq[String => Boolean](
		_.startsWith("META-INF/"),
		_.startsWith("about.html")
	)

	val svcBlankLineRegex = Pattern.compile("^\\s*(?:#.*)?$")

	def makeJar(appJar: Path, libJars: Seq[Path], outJar: Path, filters: Seq[String => Boolean], baseDir: Path,
	            javaHome: Option[Path]) {
		val services = new collection.mutable.HashMap[String, StringBuilder]
		val buffer = new Array[Byte](8192)

		def mergeServices(path: String, input: JarInputStream): Boolean = {
			if (path.startsWith("META-INF/services/")) {
				val sb = services.getOrElseUpdate(path, new StringBuilder)
				for (line <- scala.io.Source.fromInputStream(input).getLines)
					if (!svcBlankLineRegex.matcher(line).matches)
						sb.append(line).append("\n")
				true
			} else {
				false
			}
		}

		def addJarFile(output: JarOutputStream, inJar: Path) {
			val input = new JarInputStream(Files.newInputStream(inJar))
			try {
				addJarStream(output, input)
			} finally {
				input.close
			}
		}

		def addJarStream(output: JarOutputStream, input: JarInputStream) {
			Stream.continually(input.getNextJarEntry)
					.takeWhile(_ != null)
					.filter(!_.isDirectory)
					.foreach(entry => {
				val path = entry.getName
				if (!mergeServices(path, input) && !filters.exists(_(path))) {
					output.putNextEntry(new JarEntry(entry.getName))
					Stream.continually(input.read(buffer))
							.takeWhile(_ != -1)
							.foreach(count => output.write(buffer, 0, count))
					output.closeEntry
				}
				input.closeEntry
			})
		}

		val appInput = new JarInputStream(Files.newInputStream(appJar))
		try {
			val output = new JarOutputStream(new BufferedOutputStream(Files.newOutputStream(outJar)),
				appInput.getManifest)
			try {
				for (depJar <- libJars)
					addJarFile(output, depJar)

				addJarStream(output, appInput)

				for (svc <- services) {
					svc match {
						case (path, content) => {
							output.putNextEntry(new JarEntry(path))
							new OutputStreamWriter(output).append(content).flush
							output.closeEntry
						}
					}
				}
			} finally {
				output.close
			}
		} finally {
			appInput.close
		}
	}

	val oneJarSettings: Seq[Setting[_]] = Seq(
		oneJarOutput <<= (target, name, version) {
			(t, n, v) => t.toPath.resolve(n + '-' + v + ".jar")
		},
		oneJarFilters := defaultFilters,
		oneJarTask <<= (packageBin in Compile, dependencyClasspath in Compile, oneJarOutput, oneJarFilters,
				baseDirectory, javaHome) map {
			(appJar, libJars, outJar, filters, baseDir, javaHome) =>
				makeJar(appJar.toPath, libJars.files.map(_.toPath), outJar, filters, baseDir.toPath,
					javaHome.map(_.toPath))
				outJar.toFile
		}
	)
}
