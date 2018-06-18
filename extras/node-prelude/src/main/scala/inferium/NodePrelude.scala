package inferium

import ujson.Js


object NodePrelude {
    def source: String = NodePreludeSource.content
    def json: PreludeData = {
        val data = upickle.json.read(source)
        val obj = data.obj

        PreludeData(
            obj("global").obj,
            obj("modules").arr,
            obj("types").arr
        )
    }
}
