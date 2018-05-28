/*
    name: break do-while loop
    desc: break should jump out of do-while loop
 */

do {
    debug.liveCode()
    break
    debug.deadCode()
} while (debug(debug.boolean).deadCode())

debug.liveCode()

fst: do {
    debug.liveCode()
    do {
        debug.liveCode()
        break fst
        debug.deadCode()

    } while (debug(debug.boolean).deadCode())

    debug.deadCode()

} while (debug(debug.boolean).deadCode())
