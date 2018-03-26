package inferium.lattice

import org.scalatest.FlatSpec

import scala.collection.mutable
import scala.util.Random

class FuzzyHeapTest extends FlatSpec {

    type State = (Heap, List[Entity])
    private var scopeObject: ObjLocation = _
    private val nodeQueue = mutable.Queue.empty[(Node, State)]

    private def notifyNode(node: Node, state: State): Unit = {
        nodeQueue.enqueue((node, state))
    }

    private def mergeState(a: State, b: State): State = {
        val (ah, as) = a
        val (bh, bs) = b
        (ah unify bh, (as, bs).zipped map {_ unify _})
    }

    private def fixpointMergeState(a: State, b: State): State = {
        val (ah, as) = a
        val (bh, bs) = b
        (ah fixpointUnify bh, (as, bs).zipped map {_ unify _})
    }

    sealed abstract class Node {
        private var _hasPerd = false

        def connectedToPred: Boolean = _hasPerd
        def connectedToPred_=(value: Boolean): Unit = {
            assert(!_hasPerd)
            _hasPerd = true
        }

        val loc = Location()
        def processNewInState(state: State): Unit
    }

    sealed trait NextNode {
        private var _nextNode: Node = _
        def nextNode: Node = {
            assert(nextNode != null)
            _nextNode
        }

        def nextNode_=(node: Node): Unit = {
            assert(node != null)
            assert(_nextNode == null)
            _nextNode = node
            node.connectedToPred = true
        }
    }

    sealed abstract class LinearNode extends Node with NextNode {
        private var inState: State = _
        private var outState: State = _


        override def processNewInState(state: State): Unit = {
            if (inState != state) {
                inState = state
                val newOutHeap = inferState(inState._1, inState._2)

                if (newOutHeap != outState) {
                    outState = newOutHeap
                    notifyNode(nextNode, outState)
                }
            }
        }

        def inferState(heap: Heap, stack: List[Entity]): State
    }

    case class ConstantNode(entity: Entity) extends LinearNode {
        override def inferState(heap: Heap, stack: List[Entity]): (Heap, List[Entity]) = {
            (heap, entity :: stack)
        }
    }

    case class NewObject(objLocation: Location) extends LinearNode {
        override def inferState(heap: Heap, stack: List[Entity]): (Heap, List[Entity]) = {
            val m = heap.begin(loc)
            val obj = m.createObject(objLocation)
            (heap.end(m), obj :: stack)
        }
    }

    case class PropertyWriteNode(property: String) extends LinearNode {
        override def inferState(heap: Heap, stack: List[Entity]): State = {
            val target :: value :: rest = stack
            val m = heap.begin(loc)
            m.writeProperty(target, property, value)
            (heap.end(m), rest)
        }
    }

    case class PropertyReadNode(property: String) extends LinearNode {
        override def inferState(heap: Heap, stack: List[Entity]): (Heap, List[Entity]) = {
            val target :: rest = stack
            val m = heap.begin(loc)
            val value = m.readProperty(target, property)
            (heap.end(m), value :: rest)
        }
    }

    class SplitNode extends Node {
        var _thenBranch: Node = _
        var _elseBranch: Node = _

        def thenBranch: Node = _thenBranch
        def thenBranch_=(node: Node): Unit = {
            assert(node != null)
            assert(_thenBranch == null)
            _thenBranch = node
        }

        def elseBranch: Node = _elseBranch
        def elseBranch_=(node: Node): Unit = {
            assert(node != null)
            assert(_elseBranch == null)
            _elseBranch = node
        }

        override def processNewInState(state: State): Unit = {
            assert(thenBranch != null)
            assert(elseBranch != null)

            val (heap, stack) = state

            notifyNode(thenBranch, (heap.split(), stack))
            notifyNode(elseBranch, (heap.split(), stack))
        }
    }

    class MergeNode extends Node with NextNode {
        class Merger private[MergeNode] (_other: => Merger) extends Node {
            private lazy val other = _other
            private var myState: State = _
            override def processNewInState(state: State): Unit = {
                myState = state
                val otherState = Option(other).map(_.myState)
                val resultState = otherState.map(mergeState(myState, _)).getOrElse(myState)
                notifyNode(nextNode, resultState)
            }
        }

        val fst: Merger = new Merger(snd)
        val snd: Merger = new Merger(fst)

        override def processNewInState(state: (Heap, List[Entity])): Unit = throw new IllegalAccessException("This should not be called")
    }

    class LoopNode extends Node with NextNode {
        private var _loopEntry: Node = _
        private var fixpointState: State = _

        def loopEntry: Node = _loopEntry
        def loopEntry_=(node: Node): Unit = {
            assert(node != null)
            assert(_loopEntry == null)
            _loopEntry = node
        }

        class LoopExit extends Node {
            override def processNewInState(state: State): Unit = {
                val merged = fixpointMergeState(fixpointState, state)
                if (fixpointState == merged) {
                    notifyNode(nextNode, fixpointState)
                } else {
                    notifyNode(loopEntry, fixpointState)
                }
            }
        }

        val loopExit: LoopExit = new LoopExit

        override def processNewInState(state: State): Unit = {
            assert(loopExit != null)
            assert(loopEntry != null)
            if (state != fixpointState) {
                fixpointState = state
                notifyNode(loopEntry, state)
            }
        }
    }

    class EndNode extends Node {
        override def processNewInState(state: (Heap, List[Entity])): Unit = ()
    }

    def run(node: Node, state: State): Unit = {
        notifyNode(node, state)

        run()
    }

    private def run(): Unit = {
        while (nodeQueue.nonEmpty) {
            val (node, state) = nodeQueue.dequeue()

            node.processNewInState(state)
        }
    }

    val maxDepth = 2
    val minStatements = 2
    val maxStatements = 4
    val minBeginPropertyWrites = 1
    val maxIfs = 3

    private class Env {
        var ifs = 0
    }

    private def buildTestGraph(depth: Int, env: Env): (Node, NextNode) = {
        val statements = Random.nextInt(maxStatements - minStatements) + minStatements

        val nodeSeqs: IndexedSeq[Seq[Node]] = for (i <- 0 to statements) yield {
            if (Random.nextFloat() <= 0.7 || i < minBeginPropertyWrites) {
                // property write
                Seq(
                    PropertyWriteNode("test")
                )
            } else if (Random.nextFloat() <= 0.7 && env.ifs < maxIfs) {
                // if then else
                env.ifs += 1

                val consequence = buildTestGraph(depth + 1, env)
                val alternative = buildTestGraph(depth + 1, env)

                val split = new SplitNode
                split.thenBranch = consequence._1
                split.elseBranch = alternative._1

                val merge = new MergeNode

                consequence._2.nextNode = merge.fst
                alternative._2.nextNode = merge.snd

                Seq(split, merge)
            } else {
                // loop
                val (begin, end) = buildTestGraph(depth + 1, env)
                val loop = new LoopNode

                loop.loopEntry = loop
                end.nextNode = loop.loopExit

                Seq(
                    loop
                )
            }
        }

        val nodes = nodeSeqs.flatten

        nodes.reduce {
            case (in: NextNode, out) =>
                in.nextNode = out
                out
            case (_, out) =>
                out
        }

        (nodes.head, nodes.last.asInstanceOf[NextNode])
    }

    def buildTestGraph(): Node = {
        val (begin, end) = buildTestGraph(0, new Env)
        end.nextNode = new EndNode
        return begin
    }
}
