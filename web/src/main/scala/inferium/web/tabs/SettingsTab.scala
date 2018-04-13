package inferium.web.tabs

import com.thoughtworks.binding.{Binding, dom}
import inferium.web.IndexMain
import inferium.web.components.{BoundInput, Tabs}
import org.scalajs.dom.html.Element

class SettingsTab extends Tabs.Tab {



    @dom
    override def render: Binding[Element] = {
        val curSubject = IndexMain.curSubject.bind
        <div class="row">
            <div class="col s6 input-field">
                {
                    BoundInput(curSubject.name, id = s"$id-name").bind
                }
                <label for={s"$id-name"} class="active">Name</label>
            </div>
            <div class="col s12 input-field">
                {
                    BoundInput(curSubject.desc, id = s"$id-desc").bind
                }
                <label for={s"$id-desc"} class="active">Name</label>
            </div>
        </div>
    }

    override def id: String = "settings-tab"
    override def name: String = "Settings"
}
