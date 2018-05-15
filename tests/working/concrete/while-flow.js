/*
    name: while control flow
    desc: If the condition is concrete, while should disregard the respective branch
 */


var t = true
var f = false
var b = debug.boolean

while(f) {
    debug.deadCode()
}

debug.liveCode()

while(b) {
    debug.liveCode()
}

debug.liveCode()

while(t) {
    debug.liveCode()
}

debug.deadCode()

