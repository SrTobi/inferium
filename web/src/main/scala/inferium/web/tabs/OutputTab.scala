package inferium.web.tabs

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import inferium.dataflow.{DataFlowAnalysis, DebugAdapter}
import inferium.dataflow.graph.{Graph, Node, ScriptGraph}
import inferium.dataflow.graph.visitors.{DotPrintVisitor, PrintVisitor}
import inferium.prelude.NodeJs
import inferium.web.IndexMain
import inferium.web.IndexMain.Compiled
import inferium.web.components.Tabs
import inferium.utils.Utils._
import org.scalajs.dom.html.Element
import org.scalajs.dom.raw.Event

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.Try

class OutputTab extends Tabs.Tab {
    override def id: String = "output-tab"

    override def name: String = "Output"

    val lines: Vars[String] = Vars.empty
    val running: Var[Boolean] = Var(false)
    val curGraph: Var[Option[ScriptGraph]] = Var(None)

    def notifyNewGraph(f: Future[IndexMain.Compiled]): Unit = {
        /*def elem = js.Dynamic.global.document.getElementById("graph-output")

        elem.innerHTML =
            """
              |<div class="progress">
              |  <div class="indeterminate"></div>
              |</div>
            """.stripMargin

        val graphF = f map {
            case (_, _, graph) =>
                println("print dot...")
                new DotPrintVisitor(false).start(graph).toString
        }

        graphF.failed foreach {
            e =>
                elem.innerHTML =
                    s"""
                       |<div style="color: red">${e.getMessage}</div>
                     """.stripMargin
        }

        graphF foreach {
            dotCode =>
                val viz = js.Dynamic.global.Viz
                val svgPanZoom = js.Dynamic.global.svgPanZoom

                val svg = viz(dotCode, js.Dynamic.literal("format" -> "svg"))
                elem.innerHTML = svg
                val svgElem = js.Dynamic.global.document.getElementById("graph-output").getElementsByTagName("svg").item(0)
                svgPanZoom(svgElem, js.Dynamic.literal("controlIconsEnabled" -> true, "fit" -> true))
        }*/

        f.failed foreach( e => {
            lines.value.clear()
            lines.value += e.toString
            curGraph.value = None
        })

        val analizedF = f map {
            case (code, ast, graph) =>
                lines.value.clear()
                lines.value += "Compiled."
                curGraph.value = Some(graph)

        }
    }


    object OutputDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = lines.value += ("Error: " + message)

        override def warn(node: Node, message: String): Unit = lines.value +=  ("Warn: " + message)

        override def info(node: Node, message: String): Unit = lines.value +=  message

        override def hasError: Boolean = ???
    }

    def analize(): Unit = {
        if (!running.value) {
            curGraph.value foreach {
                g =>
                    running.value = true
                    lines.value.clear()
                    lines.value += "Analyzing..."

                    val p = Promise[Unit]

                    p.complete(Try {
                        val analysis = new DataFlowAnalysis(g, OutputDebugAdapter)

                        analysis.runAnalysis(NodeJs.initialState(IndexMain.inferiumConfig))
                    })

                    val f = p.future

                    f.failed foreach { f =>
                        running.value = false
                        lines.value += f.toString
                    }

                    f.foreach { _ =>
                        running.value = false
                        lines.value += "done."
                    }
            }
        }
    }

    def printSimple(): Unit = {
        curGraph.value foreach {
            g =>
                lines.value.clear()
                lines.value ++= PrintVisitor.print(g).split('\n')
        }
    }

    def printFull(): Unit = {
        curGraph.value foreach {
            g =>
                lines.value.clear()
                lines.value ++= PrintVisitor.print(g, showStackInfo = true, showNodeInfo = true, printMergeNodes = true).split('\n')
        }
    }

    @dom
    override def render: Binding[Element] = {
        <div class="row">
            <div class="col s12">
                <button type="button" class="waves-effect waves-light btn" onclick={_: Event => analize()} disabled={curGraph.bind.isEmpty || running.bind}>{if (running.bind) "Running..." else "Run" }</button>
                <button type="button" class="waves-effect waves-light btn" onclick={_: Event => printSimple()} disabled={curGraph.bind.isEmpty || running.bind}>Print</button>
                <button type="button" class="waves-effect waves-light btn" onclick={_: Event => printFull()} disabled={curGraph.bind.isEmpty || running.bind}>Print Full</button>
                <div style="font-family: monospace">
                    {
                        for (line <- lines)
                            yield {<div style={line.startsWith("  #") ?: "color: green"}>{ line.replace(' ', ' ') + " "} </div>}
                    }
                </div>
            </div>
        </div>
    }
}
