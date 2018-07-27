package inferium.dataflow.graph

import inferium.dataflow.graph.BinaryOperatorNode.BinaryOperation
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.TransformerNode
import inferium.lattice._

class BinaryOperatorNode(val op: BinaryOperation)(implicit _info: Node.Info) extends TransformerNode()(_info) {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val right :: left :: restStack = state.stack
        val result = op(left, right)
        state.copy(stack = result :: restStack)
    }

    override def asAsmStmt: String = s"binary [$op]"
}

object BinaryOperatorNode {
    sealed abstract class BinaryOperation {
        // todo: pass mutator and improve operations
        def apply(left: Entity, right: Entity): Entity
    }

    final class NumericOperation(op: (Long, Long) => Long, name: String) extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity): NumberValue = (left, right) match {
            case (SpecificNumberValue(left), SpecificNumberValue(right)) => SpecificNumberValue(op(left, right))
            case _ => NumberValue
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
        override def apply(left: Entity, right: Entity): NumberValue = (left, right) match {
            case (SpecificNumberValue(left), SpecificNumberValue(right)) => toSpecificNum(math.pow(left, right))
            case _ => NumberValue
        }

        override def toString: String = "**"
    }


    object `/` extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity): NumberValue = (left, right) match {
            case (SpecificNumberValue(left), SpecificNumberValue(right)) => toSpecificNum(left.toDouble / right.toDouble)
            case _ => NumberValue
        }

        override def toString: String = "**"
    }

    object `+` extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity): Entity = (left, right) match {
            case (SpecificNumberValue(left), SpecificNumberValue(right)) => SpecificNumberValue(left + right)
            case (SpecificNumberValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
            case (SpecificNumberValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
            case (SpecificStringValue(left), SpecificStringValue(right)) => SpecificStringValue(left + right)
            case _ => UnionValue(StringValue, NumberValue)
        }

        override def toString: String = "+"
    }

    final class NumericCompareOperation(op: (Long, Long) => Boolean, name: String) extends BinaryOperation {
        //noinspection VariablePatternShadow
        override def apply(left: Entity, right: Entity): BoolValue = (left, right) match {
            case (SpecificNumberValue(left), SpecificNumberValue(right)) => BoolValue(op(left, right))
            case _ => BoolValue
        }

        override def toString: String = name
    }

    val `<` = new NumericCompareOperation(_ < _, "<")
    val `<=` = new NumericCompareOperation(_ <= _, "<=")
    val `>` = new NumericCompareOperation(_ > _, ">")
    val `>=` = new NumericCompareOperation(_ >= _, ">=")

    object `in` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = BoolValue
        override def toString: String = "in"
    }

    object `instanceof` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = BoolValue
        override def toString: String = "instanceof"
    }

    object `==` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = BoolValue
        override def toString: String = "=="
    }

    object `===` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = BoolValue
        override def toString: String = "==="
    }

    object `!=` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = BinaryOperatorNode.`==`(left, right)
        override def toString: String = "!="
    }

    object `!==` extends BinaryOperation {
        override def apply(left: Entity, right: Entity): BoolValue = `===`(left, right).negate
        override def toString: String = "!=="
    }
}