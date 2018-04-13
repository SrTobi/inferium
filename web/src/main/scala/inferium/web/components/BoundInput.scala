package inferium.web.components

import com.thoughtworks.binding.Binding.Var
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html
import org.scalajs.dom.raw.Event

object BoundInput {
    @dom
    def apply(value: Var[String], id: String = ""): Binding[html.Input] = {
        <input
            type="text"
            id={id}
            value={value.bind}
            oninput={(e: Event) => value.value = e.target.asInstanceOf[html.Input].value}
            />
    }
}