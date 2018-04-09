package inferium.debug

import inferium.{Config, InferiumConfig}

case class Fixture(name: String, description: String, config: Config, code: String)

object Fixture {
    def fromSource(content: String): Fixture = {
        val commentBegin = content.indexOf("/*")
        require(commentBegin >= 0, "Could not find prolog begin")

        val commentEnd = content.indexOf("*/", commentBegin)
        require(commentBegin >= 0, "Could not find prolog end")

        val prolog = content.substring(commentBegin + 2, commentEnd - commentBegin - 2)
        val code = content.substring(commentEnd + 2)

        val (config, localSettings) = InferiumConfig.parseWithFreeEntries(prolog)

        val name = localSettings getOrElse("name", throw new Exception("Fixture prolog does not specify a name"))
        val description = localSettings getOrElse("desc", throw new Exception("Fixture prolog does not specify a description"))

        Fixture(name, description, config, code)
    }
}