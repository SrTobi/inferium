package inferium

import inferium.dataflow.GraphBuilder

object InferiumConfig extends Config.Definition {
    override val sections: Seq[Config.Section] = Seq(GraphBuilder.Config)
}
