package inferium.dataflow

import escalima.ast

import scala.util.Try


class BuildException(message: String, val where: Option[ast.Node] = None) extends Exception(message)
object BuildException {
    def unapply(e: BuildException): Option[(String, Option[ast.Node])] = Some(e.getMessage, e.where)

    def enrich[T](where: ast.Node)(action: => T): T = {
        try {
            action
        } catch {
            case BuildException(msg, None) => throw new BuildException(msg, Some(where))
        }
    }
}