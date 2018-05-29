/*
    name: for-loop continue
    desc: continue restarts the most inner or labeled for-loop
 */

for (let a = true; debug.boolean; c = false) {
    debug(a).isOneOf(debug.boolean)
    continue
    debug.deadCode()
}

debug.liveCode()

for (let a = "init"; debug.boolean; debug(a).isOneOf("changed")) {
    debug(a).isOneOf("init", "changed")
    a = "changed"
    continue
    debug.deadCode()
}

debug.liveCode()

for (let b = "init"; debug.boolean; b = "update") {
    debug(b).isOneOf("init", "update")

    for (let b = true; debug.boolean; b = false) {
        debug(b).isOneOf(debug.boolean)
        continue
        debug.deadCode()
    }

    debug.liveCode()
}

debug.liveCode()

let c = "init"
fst: for (debug(c).isOneOf("init"); debug.boolean; c = "update") {
    debug(c).isOneOf("init", "update")
    continue fst
    debug.deadCode()
}

debug.liveCode()

let d = "init"
snd: for (debug(d).isOneOf("init"); debug.boolean; d = "update") {
    debug(d).isOneOf("init", "update")

    inner: for (d = "init2" ;debug.boolean; debug("never").deadCode()) {
        debug(d).isOneOf("init2")
        d = "changed"
        continue snd
        debug.deadCode()
    }

    debug(d).isOneOf("init2")
    debug.liveCode()
}

debug.liveCode()