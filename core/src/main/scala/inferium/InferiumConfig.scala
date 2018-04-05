package inferium

import inferium.dataflow.GraphBuilder

class InferiumConfig extends Config.Definition {
    override val sections: Seq[Config.Section] = Seq(GraphBuilder.Config)
}
