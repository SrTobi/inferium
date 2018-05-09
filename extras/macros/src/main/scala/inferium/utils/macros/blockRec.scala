package inferium.utils.macros

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.mutable
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class blockRec(default: Any = (), nonrec: Boolean = false) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro RecursionImpl.impl
}


object RecursionImpl {
    class RecursionSet extends mutable.HashSet[Object]

    def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
        import c.universe._

        val recursionSet = q"inferium.utils.macros.RecursionImpl.RecursionSet"

        lazy val annotationParams: List[Tree] = c.prefix.tree match {
            case q"new $name( ..$params )" => params
            case _ => c.abort(c.enclosingPosition, "Annotation must provide a default value")
        }

        def getAnnotationParam(name: String, pos: Int): Option[Tree] = {

            var hadParamName = annotationParams exists {
                case q"$name = $_" => true
                case _ => false
            }

            if (hadParamName) {
                annotationParams collectFirst {
                    case q"$pname = $value" if pname.toString() == name => value
                }
            } else {
                annotationParams lift pos
            }
        }

        def default: Option[Tree] = getAnnotationParam("default", 0) orElse {
            if (getAnnotationParam("nonrec", -1) collectFirst { case q"true" => } isEmpty) {
                c.abort(c.enclosingPosition, "Either specify a default value or set nonrec = true")
            }

            None
        }


        def makeBody(body: Tree): Tree = default match {
            case Some(default) =>
                q"""
                   class Site(val ctx: AnyRef) {
                       override def hashCode: Int = ctx.hashCode
                       override def equals(o: scala.Any): Boolean = o match { case other: Site if other.ctx == ctx => true case _ => false}
                   }
                   val site = new Site(this)
                   if(calledSites.add(site)) {
                       try {
                            $body
                       } finally {
                           calledSites.remove(site)
                      }
                   } else {
                       $default
                   }
                """

            case None => body
        }

        c.Expr[Any](annottees map (_.tree) head match {
            case q"""$mods def $name(..$args): $ty""" =>
                q"""$mods def $name(..$args)(implicit calledSites: inferium.utils.macros.RecursionImpl.RecursionSet = new inferium.utils.macros.RecursionImpl.RecursionSet): $ty"""

            case q"""$mods def $name(..$args): $ty = $body""" =>
                q"""
                    $mods def $name(..$args)(implicit calledSites: inferium.utils.macros.RecursionImpl.RecursionSet = new inferium.utils.macros.RecursionImpl.RecursionSet): $ty =
                       ${makeBody(body)}
                 """


            case _ => c.abort(c.enclosingPosition, "Can only annotate methods")
        })
    }
}