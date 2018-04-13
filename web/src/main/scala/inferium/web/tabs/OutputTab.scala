package inferium.web.tabs

import com.thoughtworks.binding.Binding.{BindingSeq, Constants}
import com.thoughtworks.binding.{Binding, dom}
import inferium.web.components.Tabs
import org.scalajs.dom.html.Element

class OutputTab extends Tabs.Tab {
    override def id: String = "output-tab"

    override def name: String = "Output"

    val lines: BindingSeq[String] = Constants("Test...")

    @dom
    override def render: Binding[Element] = {
        <div class="row">
            <div class="col s12">
                {
                    for (line <- lines)
                        yield {<div>{ line }</div>}
                }
            </div>
        </div>
    }
}
