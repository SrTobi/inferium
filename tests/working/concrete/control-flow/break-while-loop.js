/*
    name: loop break
    desc: break should break the current loop
 */


while (true) {
    debug.liveCode()
    break
    debug.deadCode()
}


while (debug.boolean) {
    debug.liveCode()

    while (true) {
        debug.liveCode()
        break
        debug.deadCode()
    }
    debug.liveCode()
}


debug.liveCode()

fst: while(true) {
    debug.liveCode()
    break fst
    debug.deadCode()
}

debug.liveCode()

snd: while(true) {
    debug.liveCode()
    inner: while(true) {
        debug.liveCode()
        break snd
        debug.deadCode()
    }
    debug.deadCode()
}

debug.liveCode()


