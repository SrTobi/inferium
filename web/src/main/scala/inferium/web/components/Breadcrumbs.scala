package inferium.web.components

import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Constants, Var}
import com.thoughtworks.binding.{Binding, dom}
import inferium.web.components.TreeView.File
import org.scalajs.dom.html

class Breadcrumbs {

    private val _file: Var[Option[TreeView.File[_]]] = Var(None)
    def file: Option[TreeView.File[_]] = _file.value
    def file_=(none: None.type): Unit = _file.value = None
    def file_=(file: TreeView.File[_]): Unit = _file.value = Some(file)

    @dom
    def render: Binding[html.Div] = {
        <div class="col s12">
            {
                Constants[TreeView.File[_]](_file.bind.toSeq: _*).flatMap { file => Constants(file.path.bind.split('/').toSeq: _*) } map {
                    part =>
                        <a href="#!" class="breadcrumb">{part}</a>
                }
                //fileToBreadcrumbs(_file.bind)
            }
        </div>
    }

    /*@dom
    private def fileToBreadcrumbs(fileOpt: Option[TreeView.File[_]]) = {
        Constants(fileOpt.toSeq.flatMap{ _.path.split('/').toSeq }: _*) map {
            part =>
                <a href="#!" class="breadcrumbs">{part}</a>
        }
    }*/
}
