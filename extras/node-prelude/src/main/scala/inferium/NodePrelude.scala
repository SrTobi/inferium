package inferium

import ujson.Js


object NodePrelude {
    def source: String = NodePreludeSource.content
    def json: PreludeData = {
        val data = upickle.json.read(source)

        data match {
            case Js.Obj(obj) =>
                List("global", "modules", "types") flatMap { obj get _ } match {
                    case (global@Js.Obj(_)) :: (modules@Js.Arr(_)) :: (types@Js.Arr(_)) :: List.empty =>
                        PreludeData(global, modules, types)
                }
        }

    }
}
