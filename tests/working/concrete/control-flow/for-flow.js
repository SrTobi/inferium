/*
    name: for flow
    desc: for-loops should handle init-, test- and update-expressions correctly
 */


for (var a = "init"; debug.boolean; a = "update") {
    debug(a).is("init", "update")
    a = "inner"
    debug(a).is("inner")
}

debug(a).is("init", "update")

let b = "outer"

for (let b = "init"; debug.boolean; debug(b).is("init", "inner1")) {
    debug(b).is("init", "inner1")
    b = "inner1"
    let b = "inner2"
    debug(b).is("inner2")
}

debug(b).is("outer")


var c = false

for (c = true; c; c = false) {
    debug(c).is(true)
}

debug(c).is(false)

for (; debug.boolean;) {
    debug.liveCode()
}

debug.liveCode()

for (;;) {}

debug.deadCode()
