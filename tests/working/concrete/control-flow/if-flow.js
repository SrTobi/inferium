/*
    name: if control flow
    desc: If the condition is concrete, if should disregard the false branch
 */

var t = true
var f = false
var b = debug.boolean
var result

if(t) {
    debug.liveCode()
} else {
    debug.deadCode()
}

if(f) {
    debug.deadCode()
} else {
    debug.liveCode()
}

if(b) {
    debug.liveCode()
} else {
    debug.liveCode()
}