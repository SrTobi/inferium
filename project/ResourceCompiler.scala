import sbt.Keys.TaskStreams
import sbt.{Def, _}
import java.io.{File => JFile}

object ResourceCompiler {
    private def generateResourceFile(targetManaged: File, resourcePath: File, resources: Set[File], pkg: String, name: String): Set[JFile] = {
        val builder = new StringBuilder

        println("read " + resourcePath)
        val target = targetManaged / pkg.replaceAllLiterally(".", "/") / s"$name.scala"
        println("write to " + target)
        println(resources)

        builder.append(s"package $pkg")
        builder.append("\n")
        builder.append(s"object $name {\n")
        builder.append("    val files: Map[String, String] = Map(\n")
        for (res <- resources.iterator) {
            val filename = res.getCanonicalPath.substring(resourcePath.getCanonicalPath.length.toInt + 1)
            println("Generating: " + filename)
            val content = IO.read(res).replaceAllLiterally("$", "$$")
            builder.append("        \"" + filename + "\" -> raw\"\"\"" + content + "\"\"\",\n")
        }
        builder.append("    )\n")
        builder.append("}\n")
        IO.write(target, builder.toString)

        Set(targetManaged)
    }

    def bundle(source: File, targetFile: File, pkg: String, name: String): Unit = {
        val files = Path.allSubpaths(source) map { _._1 }
        generateResourceFile(targetFile, source, files.toSet, pkg, name)
    }

    /*def buildResource(source: File, targetFile: File, cache: File): Seq[JFile] = {

        println("read from " + source)
        println("write to " + targetFile)
        println("cache in " + cache)

        val files = Path.allSubpaths(source) map { _._1 }

        val cached = FileFunction.cached(
            cache,
            inStyle = FilesInfo.lastModified,
            outStyle = FilesInfo.exists) {
            (resources: Set[JFile]) =>
                generateResourceFile(targetFile, source, resources)
        }

        cached(files.toSet).toSeq
    }*/
}