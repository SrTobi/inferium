package jseval

private object JSEvalBridgeCreator {
    def create(): JSEvalBridge = new JS_JSEvalBridge()
}
