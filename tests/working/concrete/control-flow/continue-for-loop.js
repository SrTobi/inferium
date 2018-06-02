/*
    name: for-loop continue
    desc: continue restarts the most inner or labeled for-loop
 */

for (let a = true; debug.boolean; a = false) {
    debug(a).is(debug.boolean)
    continue
    debug.deadCode()
}

debug.liveCode()

for (let a = "init"; debug.boolean; debug(a).is("changed")) {
    debug(a).is("init", "changed")
    a = "changed"
    continue
    debug.deadCode()
}

debug.liveCode()

for (let b = "init"; debug.boolean; b = "update") {
    debug(b).is("init", "update")

    for (let b = true; debug.boolean; b = false) {
        debug(b).is(debug.boolean)
        continue
        debug.deadCode()
    }

    debug.liveCode()
}

debug.liveCode()

let c = "init"
fst: for (debug(c).is("init"); debug.boolean; c = "update") {
    debug(c).is("init", "update")
    continue fst
    debug.deadCode()
}

debug.liveCode()

let d = "init"
snd: for (debug(d).is("init"); debug.boolean; d = "update") {
    debug(d).is("init", "update")

    inner: for (d = "init2" ;debug.boolean; debug("never").deadCode()) {
        debug(d).is("init2")
        d = "changed"
        continue snd
        debug.deadCode()
    }

    debug(d).is("init2")
    debug.liveCode()
}

debug.liveCode()