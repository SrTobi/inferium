/*
    name: for flow
    desc: for-loops should handle init-, test- and update-expressions correctly
 */


for (var a = "init"; debug.boolean; a = "update") {
    debug(a).isOneOf("init", "update")
    a = "inner"
    debug(a).isOneOf("inner")
}

debug(a).isOneOf("init", "update")

let b = "outer"

for (let b = "init"; debug.boolean; debug(b).isOneOf("init", "inner1")) {
    debug(b).isOneOf("init", "inner1")
    b = "inner1"
    let b = "inner2"
    debug(b).isOneOf("inner2")
}

debug(b).isOneOf("outer")


var c = false

for (c = true; c; c = false) {
    debug(c).isOneOf(true)
}

debug(c).isOneOf(false)

for (; debug.boolean;) {
    debug.liveCode()
}

debug.liveCode()

for (;;) {}

debug.deadCode()
