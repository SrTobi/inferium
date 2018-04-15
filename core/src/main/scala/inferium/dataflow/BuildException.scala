package inferium.dataflow

import escalima.ast

import scala.util.Try
import scala.util.control.NonFatal


class BuildException(message: String, private var _where: Option[ast.Node] = None) extends Exception(message) {
    def this(message: String, _where: Option[ast.Node], cause: Throwable) = {
        this(message, _where)
        initCause(cause)
    }

    def where: Option[ast.Node] = _where
    override def getMessage: String = {
        val pos = where flatMap { _.loc } map { case ast.SourceLocation(_, ast.Position(line, column), _) => s"$line:$column" } getOrElse "unknown"
        s"($pos): ${super.getMessage}"
    }
}

object BuildException {
    def unapply(e: BuildException): Option[(String, Option[ast.Node])] = Some(e.getMessage, e.where)

    def enrich[T](where: ast.Node)(action: => T): T = {
        try {
            action
        } catch {
            case e@BuildException(msg, whereOpt) =>
                e._where = whereOpt orElse Some(where)
                throw e

            case NonFatal(e) =>
                throw new BuildException(s"Internal builder problem: $e", Some(where), e)
        }
    }
}