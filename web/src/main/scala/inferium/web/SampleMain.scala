package inferium.web

import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.html._
import org.scalajs.dom.{Node, document}
import org.scalajs.dom.raw.Event

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel(name = "SampleMain")
object SampleMain {

    case class Contact(name: Var[String], email: Var[String])

    val data = Vars.empty[Contact]

    @dom
    def table: Binding[BindingSeq[Node]]= {
        <header>
            <nav id="top-nav">
                <div class="container">
                    <div class="nav-wrapper">
                        <a class="page-title">
                            This is a test
                        </a>
                    </div>
                </div>
            </nav>
        </header>
        <div class="container">
            <div>
                <button
                    class="btn red"
                    onclick={ event: Event =>
                        data.value += Contact(Var("XXX"), Var("blub blub"))
                    }
                >
                    Add a contact
                </button>
            </div>
            <table border="1" cellPadding="5" class="striped">
                <thead>
                    <tr>
                        <th>Name</th>
                        <th>E-mail</th>
                        <th>Operation</th>
                    </tr>
                </thead>
                <tbody>
                    {
                    for (contact <- data) yield {
                        <tr>
                            <td>
                                {contact.name.bind}
                            </td>
                            <td>
                                {contact.email.bind}
                            </td>
                            <td>
                                <button
                                    class="btn waves-effect waves-light"
                                    onclick={ event: Event =>
                                        contact.name.value = "Modified Name"
                                    }
                                >
                                    Modify the name
                                </button>
                            </td>
                        </tr>
                    }
                    }
                </tbody>
            </table>
        </div>
    }

    @JSExport
    def main(): Unit = {
        dom.render(document.body, table)
    }

}