package inferium.prelude

import inferium.js.types.js
import inferium.prelude.data.NodeJsPreludeData
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class NodeJsPreludeSpec extends FlatSpec with Matchers {
    import js._

    "NodeJs prelude" should "be loadable" in {
        val preludeJson = NodeJsPreludeData.json
        val prelude = js.Prelude.load(preludeJson)

        val checked = mutable.Set.empty[Type]
        val toCheck = mutable.Queue.empty[Type]

        def check(ty: Type): Unit = {
            if (!checked(ty)) {
                checked += ty
                toCheck += ty
            }
        }

        def checkSignature(sig: Signature): Unit = sig foreach {
            overload =>
                overload.generics foreach { _.constraint foreach { check } }
                overload.params foreach { p => check(p.ty) }
                check(overload.returnType)
        }

        while (toCheck.nonEmpty) {
            toCheck.dequeue() match {
                case tuple: TupleType =>
                    assert(tuple.members != null)
                    tuple.members foreach { check }

                case union: UnionType =>
                    assert(union.types != null)
                    union.types foreach { check }

                case inst: Instantiate =>
                    assert(inst.typeArguments != null)
                    assert(inst.target != null)
                    inst.typeArguments foreach { check }
                    check(inst.target)

                case comp: CompoundType =>
                    checkSignature(comp.constructor)
                    checkSignature(comp.signature)
                    comp.bases foreach { check }
                    comp.properties.values foreach { p => check(p.ty) }

                case _ =>
            }
        }

    }
}
