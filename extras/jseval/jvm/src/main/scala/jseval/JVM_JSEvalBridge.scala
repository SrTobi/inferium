package jseval

import javax.script.{Invocable, ScriptEngineManager, ScriptException}

private class JVM_JSEvalBridge extends JSEvalBridge {
    private val engine = new ScriptEngineManager().getEngineByName("js")
    assert(engine != null, "Could not initialize JavaScript engine")

    private val jsonObj = engine.get("JSON")

    override def eval(source: String): String = {
        try {
            val result = engine.eval(source)
            val json = engine.asInstanceOf[Invocable].invokeMethod(jsonObj, "stringify", result)
            json.asInstanceOf[String]
        } catch {
            case e: ScriptException =>
                throw new JSEvalException(e.getMessage, e)
        }
    }
}
