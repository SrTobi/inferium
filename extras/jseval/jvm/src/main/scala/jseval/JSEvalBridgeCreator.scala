package jseval

private object JSEvalBridgeCreator {
    def create(): JSEvalBridge = new JVM_JSEvalBridge()
}
