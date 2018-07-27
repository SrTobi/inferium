package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.UnaryOperatorNode.UnaryOperation
import inferium.dataflow.graph.traits.TransformerNode
import inferium.js.types.js
import inferium.lattice._

class UnaryOperatorNode(val op: UnaryOperation)(implicit _info: Node.Info) extends TransformerNode()(_info) {
    private val accessLocation = Location()
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val heap = state.heap
        val operand :: restStack = state.stack
        val mutator = heap.begin(accessLocation)
        val result = op(operand, mutator)
        val resultHeap = heap.end(mutator)
        state.copy(stack = result :: restStack, heap = resultHeap)
    }

    override def asAsmStmt: String = s"unary [$op]"
}


object UnaryOperatorNode {
    sealed abstract class UnaryOperation {
        def apply(operand: Entity, mutator: Heap.Mutator): Entity
    }


    class NumericOperator(op: Long => Long, name: String) extends UnaryOperation {
        //noinspection VariablePatternShadow
        override def apply(operand: Entity, mutator: Heap.Mutator): NumberValue = {
            operand.asProbes(mutator).foreach { _.usedAs(js.NumberType) }
            operand match {
                case SpecificNumberValue(operand) => SpecificNumberValue(op(operand))
                case _ => NumberValue
            }
        }
        override def toString: String = name
    }

    val `-` = new NumericOperator(-_, "-")
    val `+` = new NumericOperator(+_, "-")
    val `~` = new NumericOperator(~_, "~")


    object `!` extends UnaryOperation {
        //noinspection VariablePatternShadow
        override def apply(operand: Entity, mutator: Heap.Mutator): Entity = {
            operand.asProbes(mutator).foreach { _.usedAs(js.BooleanType) }
            BoolValue(operand.asBoolLattice(mutator).negate)
        }
        override def toString: String = "!"
    }

    object `typeof` extends UnaryOperation {
        override def apply(operand: Entity, mutator: Heap.Mutator): Entity = {
            val typeofs = operand.asTypeof(mutator)
            if(typeofs contains "any") StringValue else Entity.unify(typeofs map StringValue.apply)
        }
        override def toString: String = "typeof"
    }
}