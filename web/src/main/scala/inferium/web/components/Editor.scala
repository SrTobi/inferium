package inferium.web.components

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import js.{Dynamic => Dyn}
import scala.scalajs.js.Dynamic._
import scala.concurrent.Future

/**
  * Everything related to setting up the Ace editor to
  * do exactly what we want.
  */
class Editor(hookId: String,
             bindings: Seq[(String, String, () => Any)]) {
             //completions: () => Future[Seq[(String, String)]]) {
    //lazy val Autocomplete = js.Dynamic.global.require("ace/autocomplete").Autocomplete
    def session: Dyn = editor.getSession()
    def aceDoc: Dyn = session.getDocument()
    def code: String = session.getValue().asInstanceOf[String]
    def code_=(c: String) = {
        editor.setValue(c, -1)
    }
    def row: Int = editor.getCursorPosition().row.asInstanceOf[Int]
    def column: Int = editor.getCursorPosition().column.asInstanceOf[Int]

    /*def complete() = {
        if (!js.DynamicImplicits.truthValue(editor.completer))
            editor.completer = js.Dynamic.newInstance(Autocomplete)()
        js.Dynamic.global.window.ed = editor
        editor.completer.showPopup(editor)

        // needed for firefox on mac
        editor.completer.cancelContextMenu()

    }*/

    val editor: js.Dynamic = {
        val editor = global.ace.edit(hookId)
        println(editor)
        editor.setTheme("ace/theme/dawn")
        editor.session.setMode("ace/mode/javascript")

        for ((name, key, func) <- bindings){
            val binding = s"Ctrl-$key|Cmd-$key"
            editor.commands.addCommand(js.Dynamic.literal(
                "name" -> name,
                "bindKey" -> js.Dynamic.literal(
                    "win" -> binding,
                    "mac" -> binding,
                    "sender" -> "editor|cli"
                ),
                "exec" -> func
            ))
        }

        /*editor.completers = js.Array(js.Dynamic.literal.obj(
            "getCompletions" -> {(editor: Dyn, session: Dyn, pos: Dyn, prefix: Dyn, callback: Dyn) => task*async{
                val things = await(completions()).map{ case (name, value) =>
                    JsVal.obj(
                        "value" -> value,
                        "caption" -> (value + name)
                    ).value
                }
                callback(null, js.Array(things:_*))
            }}
        ).value)*/

        editor.getSession().setTabSize(4)

        editor
    }
}