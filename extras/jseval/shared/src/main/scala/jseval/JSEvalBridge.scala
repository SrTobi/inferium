package jseval

private trait JSEvalBridge {
    def eval(source: String): String
}
