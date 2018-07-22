package inferium.web

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import escalima.ECMAScript
import inferium.Config
import inferium.dataflow.GraphBuilder
import inferium.dataflow.graph.ScriptGraph
import inferium.web.components._
import inferium.web.tabs.{GraphTab, OutputTab, SettingsTab}
import org.scalajs.dom.html.Element
import org.scalajs.dom.{Node, document}

import scala.scalajs.js.timers._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Try

import scala.concurrent._
import ExecutionContext.Implicits.global

@JSExportTopLevel(name = "IndexMain")
object IndexMain {
    val inferiumConfig: Config = Config(
        GraphBuilder.Config.buildDebugNodes := true
    )

    private val parser = new ECMAScript
    private val treeData = new TreeView.Directory[TestSubject]("/")
    private val treeView = new TreeView[TestSubject]
    private val breadcrumbs = new Breadcrumbs

    private val test = Var("hallo")
    private var editor: Editor = _

    val curSubject: Var[TestSubject] = Var(null)

    {
        def select(file: TreeView.File[TestSubject]): Unit = {
            treeView.selectedFiles.value = Set(file)

            val newSubject = file.data
            val oldSubject = curSubject.value
            if (oldSubject != null) {
                oldSubject.setEditedBy(None)
            }
            curSubject.value = file.data
            if (editor != null)
                newSubject.setEditedBy(editor)

            breadcrumbs.file = file
        }
        def file(name: String, code: String): TreeView.File[TestSubject] = {
            val nameVar = Var(name)
            val subject = new TestSubject(nameVar, code)
            new TreeView.File(nameVar, subject)
        }

        val sub = new TreeView.Directory[TestSubject]("haha")
        treeData += sub
        sub += file("inner.js", "js")
        val f = file("test.html", "// xxx.html")
        treeData  += f
        treeData  += file("xxx.html", "// test.html")
        treeData.expanded.value = true
        treeView.root.value = treeData

        treeView.onFileClick = select
        select(f)
    }

    type Compiled = (String, escalima.ast.Program, ScriptGraph)

    private def compile(lastCode: String): Unit = {
        setTimeout(3000) {
            val code = curSubject.value.code
            if (code != lastCode) {
                println("code changed")
                val p = Promise[Compiled]

                val f = p.future
                compileListeners foreach {_(f)}

                p.complete(Try {
                    val prog = parser.parseScript(code)
                    val graph = new GraphBuilder(inferiumConfig).buildTemplate(prog, Map.empty).instantiate()
                    println(graph)
                    (code, prog, graph)
                })
                f.failed foreach { println }
            }
            compile(code)
        }
    }

    compile("")

    val compileListeners: mutable.Buffer[Future[Compiled] => Unit] = mutable.Buffer.empty

    val tabs: Tabs = {
        val graphTab = new GraphTab
        compileListeners += graphTab.notifyNewGraph

        val outputTab = new OutputTab
        compileListeners += outputTab.notifyNewGraph

        new Tabs(Constants(
            new SettingsTab,
            outputTab,
            graphTab
        ))
    }

    @dom
    def table: Binding[BindingSeq[Node]]= {
        <nav>
            <div class="nav-wrapper">
                <a href="#" class="brand-logo right">Inferium</a>
                <!---<ul id="nav-mobile" class="right hide-on-med-and-down">
                    <li><a href="sass.html">Sass</a></li>
                    <li><a href="badges.html">Components</a></li>
                    <li><a href="collapsible.html">JavaScript</a></li>
                </ul>-->
                {
                    breadcrumbs.render.bind
                }
            </div>
        </nav>
        <div class="row">
            <div class="col s3 menu-area">
                {
                    treeView.render.bind
                }
            </div>
            <div class="col s3 editor-area">
                <div id="editor"></div>
            </div>
            <div class="col s6 settings-area">
                {
                    tabs.render.bind
                }
            </div>
        </div>
    }

    @JSExport
    def main(): Unit = {
        dom.render(document.body, table)
        editor = new Editor("editor", Seq())
        curSubject.value.setEditedBy(editor)
    }

}