package jseval

import upickle.Js

class JSEval {
    private val bridge: JSEvalBridge = JSEvalBridgeCreator.create()

    def eval(source: String): Js.Value = upickle.json.read(evalToJson(source))

    def evalToJson(source: String): String = bridge.eval(source)
}
