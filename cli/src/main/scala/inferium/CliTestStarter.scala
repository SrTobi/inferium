package inferium

object CliTestStarter {
    def main(args: Array[String]): Unit = {
        CliMain.main(Array("/tmp/inferium/node_modules/amplify", "/tmp/inferium/result.json"))
    }
}

