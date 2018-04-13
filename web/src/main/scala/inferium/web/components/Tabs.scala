package inferium.web.components

import com.thoughtworks.binding.Binding.BindingSeq
import com.thoughtworks.binding.{Binding, dom}
import inferium.utils.IdGenerator
import org.scalajs.dom.html

class Tabs(tabs: BindingSeq[Tabs.Tab]) {
    import Tabs._

    private val id = idGenerator.newId()
    private def idStr = "tabs${id.id}"

    @dom
    def render: Binding[html.Element] = {
        <div class="row">
            <div class="col s12">
                <ul class="tabs" id={idStr}>
                    {
                        for (tab <- tabs)
                            yield <li class="tab col s3"><a href={s"#${tab.id}"}>{ tab.name }</a></li>
                    }
                </ul>
            </div>
            {
                for (tab <- tabs) yield {
                    <div id={tab.id} class="col s12 tab-container"> {tab.render.bind} </div>
                }
            }
            <script type="text/javascript">
                {
                    s"""
                      |M.Tabs.init(document.getElementById("$idStr"));
                    """.stripMargin
                }
            </script>
        </div>
    }
}

object Tabs {
    private val idGenerator = new IdGenerator[Tabs]

    trait Tab {
        def id: String
        def name: String
        def render: Binding[html.Element]
    }
}