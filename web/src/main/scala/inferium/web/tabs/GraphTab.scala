package inferium.web.tabs

import com.thoughtworks.binding.{Binding, dom}
import inferium.dataflow.graph.visitors.DotPrintVisitor
import inferium.web.IndexMain
import inferium.web.components.Tabs
import org.scalajs.dom.html.Element
import scala.concurrent._
import ExecutionContext.Implicits.global

import scala.scalajs.js

class GraphTab extends Tabs.Tab {
    override def id: String = "graph-tab"
    override def name: String = "Graph"


    def notifyNewGraph(f: Future[IndexMain.Compiled]): Unit = {
        def elem = js.Dynamic.global.document.getElementById("graph-output")

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
        }
    }

    @dom
    override def render: Binding[Element] = {
        <div id="graph-output">
        </div>
    }
}
