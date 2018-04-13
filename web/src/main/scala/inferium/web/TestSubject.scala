package inferium.web

import com.thoughtworks.binding.Binding.Var
import inferium.web.components.Editor

class TestSubject(_name: Var[String], _code: String) {
    private var _savedCode = _code
    private var _editor: Option[Editor] = None

    val name = _name
    val desc = Var("")

    def saveCode(): Unit = {
        _savedCode = code
    }

    def setEditedBy(editor: Editor): Unit = {
        saveCode()
        _editor = Some(editor)
        editor.code = _savedCode
    }

    def setEditedBy(none: None.type): Unit = {
        saveCode()
        _editor = None
    }

    def code: String = _editor map { _.code } getOrElse _savedCode
}
