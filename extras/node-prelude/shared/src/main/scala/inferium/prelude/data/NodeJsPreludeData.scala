package inferium.prelude.data

import ujson.Js


object NodeJsPreludeData {
    def source: String = NodeJsPreludeSource.content
    def json: Js.Obj = upickle.json.read(source).asInstanceOf[Js.Obj]
}