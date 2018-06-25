package inferium.prelude.data

object NodeJsPreludeSource {
    def content: String = io.Source.fromResource("node-prelude.json").mkString
}
