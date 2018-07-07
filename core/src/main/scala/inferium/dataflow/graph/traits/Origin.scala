package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node.StateOrigin

trait Origin {
    def thisOrigin: StateOrigin
}
