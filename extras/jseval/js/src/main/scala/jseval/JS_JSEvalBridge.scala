package jseval

import scala.scalajs.js
import scala.scalajs.js.JSON

private class JS_JSEvalBridge extends JSEvalBridge {
    override def eval(source: String): String = {
        try {
            val result = js.eval(source)
            JSON.stringify(result)
        } catch {
            case e: Exception =>
                throw new JSEvalException(e.getMessage, e)
        }
    }
}
