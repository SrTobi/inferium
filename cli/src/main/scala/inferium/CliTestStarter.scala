package inferium

object CliTestStarter {
    def main(args: Array[String]): Unit = {
        CliMain.main(Array("/tmp/inferium/node_modules/bootbox", "/tmp/inferium/result.json"))
    }
}

