package inferium.dataflow.graph

import inferium.dataflow.graph.BinaryOperatorNode.BinaryOperation
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.TransformerNode
import inferium.js.types.js
import inferium.lattice._

class BinaryOperatorNode(val op: BinaryOperation)(implicit _info: Node.Info) extends TransformerNode()(_info) {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val right :: left :: restStack = state.stack
        val mutator = state.heap.begin(Location())
        val result = op(left, right, mutator)
        state.copy(stack = result :: restStack, state.heap.end(mutator))
    }

    override def asAsmStmt: String = s"binary [$op]"
}

object BinaryOperatorNode {
    sealed abstract class BinaryOperation {
        // todo: pass mutator and improve operations
        def apply(left: Entity, right: Entity, mutator: Heap.Mutator): Entity
    }

    final class NumericOperation(op: (Long, Long) => Long, name: String) extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): NumberValue = {
            left.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            right.asProbes(mutator).foreach { _.usedAs(js.NumberType) }

            (left, right) match {
                case (SpecificNumberValue(left), SpecificNumberValue(right)) => SpecificNumberValue(op(left, right))
                case _ => NumberValue
            }
        }

        override def toString: String = name
    }

    private def isLongValue(d: Double): Boolean = (d % 1) == 0
    private def toSpecificNum(d: Double): NumberValue = if (isLongValue(d)) SpecificNumberValue(d.toLong) else NumberValue

    //val `+` = new NumericOperation(_ + _)
    val `-` = new NumericOperation(_ - _, "-")
    val `*` = new NumericOperation(_ * _, "*")
    val `%` = new NumericOperation(_ % _, "%")
    val `|` = new NumericOperation(_ | _, "|")
    val `^` = new NumericOperation(_ ^ _, "^")
    val `&` = new NumericOperation(_ & _, "&")
    val `<<` = new NumericOperation(_ << _, "<<")
    val `>>` = new NumericOperation(_ >> _, ">>")
    val `>>>` = new NumericOperation(_ >>> _, ">>>")

    object `**` extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): NumberValue = {
            left.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            right.asProbes(mutator).foreach { _.usedAs(js.NumberType) }

            (left, right) match {
                case (SpecificNumberValue(left), SpecificNumberValue(right)) => toSpecificNum(math.pow(left, right))
                case _ => NumberValue
            }
        }

        override def toString: String = "**"
    }


    object `/` extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): NumberValue = {
            left.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            right.asProbes(mutator).foreach { _.usedAs(js.NumberType) }

            (left, right) match {
                case (SpecificNumberValue(left), SpecificNumberValue(right)) => toSpecificNum(left.toDouble / right.toDouble)
                case _ => NumberValue
            }
        }

        override def toString: String = "**"
    }

    object `+` extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(lef: Entity, righ: Entity, mutator: Heap.Mutator): Entity = {
            (lef, righ) match {
                case (SpecificNumberValue(left), SpecificNumberValue(right)) => SpecificNumberValue(left + right)
                case (SpecificNumberValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
                case (SpecificNumberValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
                case (SpecificStringValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
                case _ =>
                    val stringOrNum = js.UnionType(js.NumberType, js.StringType)
                    lef.asProbes(mutator).foreach { _.usedAs(stringOrNum) }
                    righ.asProbes(mutator).foreach { _.usedAs(stringOrNum) }
                    UnionValue(StringValue, NumberValue)
            }
        }

        override def toString: String = "+"
    }

    final class NumericCompareOperation(op: (Long, Long) => Boolean, name: String) extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = {
            left.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            right.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            (left, right) match {
                case (SpecificNumberValue(left), SpecificNumberValue(right)) => BoolValue(op(left, right))
                case _ => BoolValue
            }
        }

        override def toString: String = name
    }

    val `<` = new NumericCompareOperation(_ < _, "<")
    val `<=` = new NumericCompareOperation(_ <= _, "<=")
    val `>` = new NumericCompareOperation(_ > _, ">")
    val `>=` = new NumericCompareOperation(_ >= _, ">=")

    object `in` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = BoolValue
        override def toString: String = "in"
    }

    object `instanceof` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = {
            left.asProbes(mutator).foreach { _.usedAs(js.ObjectType) }
            right.asProbes(mutator).foreach { _.usedAs(js.ObjectType) }
            BoolValue
        }
        override def toString: String = "instanceof"
    }

    object `==` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = BoolValue
        override def toString: String = "=="
    }

    object `===` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = BoolValue
        override def toString: String = "==="
    }

    object `!=` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = BinaryOperatorNode.`==`(left, right, mutator)
        override def toString: String = "!="
    }

    object `!==` extends BinaryOperation {
        override def apply(left: Entity, right: Entity, mutator: Heap.Mutator): BoolValue = `===`(left, right, mutator).negate
        override def toString: String = "!=="
    }
}