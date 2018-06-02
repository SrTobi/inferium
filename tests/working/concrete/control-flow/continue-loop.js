/*
    name: loop continue
    desc: continue starts the most inner or labeled loop over
 */

while (debug.boolean) {
    debug.liveCode()
    continue
    debug.deadCode()
}

debug.liveCode()

while (debug.boolean) {
    debug.liveCode()

    while (debug.boolean) {
        debug.liveCode()
        continue
        debug.deadCode()
    }

    debug.liveCode()
}

debug.liveCode()


fst: while (debug.boolean) {
    debug.liveCode()
    continue fst
    debug.deadCode()
}

debug.liveCode()

snd: while (debug.boolean) {
    debug.liveCode()

    var a = "init"

    inner: while (debug.boolean) {
        debug.liveCode()
        debug(a).is("init")
        a = "changed"
        continue snd
        debug.deadCode()
    }

    debug(a).is("init")
    debug.liveCode()
}

debug.liveCode()

third: while (debug.boolean) {
    debug.liveCode()

    var b = "init"

    inner: while (debug.boolean) {
        debug.liveCode()
        debug(b).is("init")
        b = "changed"
        continue third
        debug.deadCode()
    }

    debug(b).is("init", "changed")
    debug.liveCode()
}

debug.liveCode()

while (true) {
    continue
}

debug.deadCode()