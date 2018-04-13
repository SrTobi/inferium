package inferium.web.components

import com.thoughtworks.binding.Binding.{BindingSeq, Constant, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import inferium.utils.{Id, IdGenerator}
import inferium.web.components.TreeView.{Directory, File, TreeNode}
import org.scalajs.dom.html
import org.scalajs.dom.html.Element
import org.scalajs.dom.raw.Event
import org.scalajs.dom.{Node => HtmlNode}

class TreeView[T] {
    type TreeNode = TreeView.TreeNode[T]
    type TreeFile = TreeView.File[T]
    type TreeDir = TreeView.Directory[T]
    val root: Var[TreeNode] = Var(new TreeDir("/"))
    var onFileClick: (TreeFile) => Unit = (_) => ()
    var onDirClick: (TreeDir) => Unit = (_) => ()

    var selectedFiles: Var[Set[TreeFile]] = Var(Set.empty)


    @dom
    def render: Binding[html.Div] = {
        <div class="tree">
            <ul>
                {displayNode(root.bind).bind}
            </ul>
        </div>
    }

    @dom
    private def displayNode(node: TreeNode): Binding[HtmlNode] = node match {
        case file: TreeFile =>
            <li>
                <label onclick={_: Event => onFileClick(file)} class={if (selectedFiles.bind.contains(file)) "selected-file" else ""} ><i class="fa fa-file"/> <span>{file.name.bind}</span></label>
            </li>
        case dir: TreeDir =>
            <li>
                <!--<i class={s"fa ${treeNodeClass(dir).bind}"}/> -->
                <!-- onclick={_: Event => onTreeNodeClick(node)} -->
                <label for={node.id} onclick={_: Event => _onDirectoryClick(dir)} > {dirNodeSymbol(dir).bind} {node.name.bind}</label>
                <ul class={if(dir.expanded.bind) "" else "hide"}>
                    {
                        for (c <- dir.children) yield displayNode(c).bind
                    }
                </ul>
            </li>
    }

    //noinspection SimplifyBooleanMatch
    @dom
    private def dirNodeSymbol(node: TreeDir): Binding[html.Element] = node.expanded.bind match {
        case true => <i class="fas fa-folder-open" />
        case false => <i class="fas fa-folder" />
    }

    private def _onDirectoryClick(dir: TreeDir): Unit = {
        dir.expanded.value = !dir.expanded.value
    }
}

object TreeView {

    private val nodeIdGen = new IdGenerator[TreeNode[_]]

    sealed abstract class TreeNode[T](val name: Var[String]) {
        private val internalId: Id[TreeNode[_]] = TreeView.nodeIdGen.newId()

        def id: String = s"treeNode${internalId.id}"
        var parent: Var[Option[Directory[T]]] = Var(None)

        @dom
        def path: Binding[String] = {
            import scalaz.std.option._
            val dir = parent.bind map { _.path.bind } getOrElse ""
            val nm = name.bind
            if (nm == "/") {
                dir
            } else {
                s"$dir/${nm}"
            }
        }

        override def equals(o: scala.Any): Boolean = o match {
            case other: TreeNode[T] => other.internalId == this.internalId
            case _ => false
        }
        override def hashCode(): Int = internalId.hashCode()
    }

    class Directory[T](_name: Var[String]) extends TreeNode[T](_name) {
        val expanded: Var[Boolean] = Var(false)
        val children: Vars[TreeNode[T]] = Vars.empty.asInstanceOf[Vars[TreeNode[T]]]

        def +=(node: TreeNode[T]): Unit = {
            node.parent.value foreach {
                _.children.value -= node
            }
            node.parent.value = Some(this)
            children.value += node
        }

        def this(_name: String) = this(Var(_name))
    }

    class File[T](_name: Var[String], val data: T) extends TreeNode[T](_name) {

        def this(_name: String, data: T) = this(Var(_name), data)
    }
}