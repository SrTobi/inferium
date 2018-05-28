/*
    name: continue do-while
    desc: continue should restart do-while loop
 */

do {
    debug.liveCode()
    continue
    debug.deadCode()

} while (debug(debug.boolean).liveCode())

debug.liveCode()

fst: do {

    debug.liveCode()

    do {
        debug.liveCode()
        continue fst
        debug.deadCode()
    } while (debug(true).deadCode())

    debug.deadCode()

} while (debug(debug.boolean).liveCode())

debug.liveCode()