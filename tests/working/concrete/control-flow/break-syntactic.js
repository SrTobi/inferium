/*
    name: syntactic break
    desc: break should jump to referenced label
 */

fst: {
    debug.liveCode()
    break fst
    debug.deadCode()
}

debug.liveCode()

snd: {
    inner: {
        debug.liveCode()
        break snd
        debug.deadCode()
    }
    debug.deadCode()
}

debug.liveCode()

outer: {
    third: {
        debug.liveCode()
        break third
        debug.deadCode()
    }
    debug.liveCode()
}

debug.liveCode()

forth:
    break forth;

debug.liveCode()

fifth:
after: {
    debug.liveCode()
    break fifth
    debug.deadCode()
}

debug.liveCode()