package inferium

import inferium.dataflow.GraphBuilder
import inferium.dataflow.GraphBuilder.Config._

object InferiumConfig extends Config.Definition {
    override val sections: Seq[Config.Section] = Seq(GraphBuilder.Config)

    object Env {
        val Debug: Config = Config(
            buildDebugNodes := true,
            debugObjectName := "debug"
        )

        val Node: Config = Config()
        val NodeDebug: Config = Node <+ Debug
    }
}
