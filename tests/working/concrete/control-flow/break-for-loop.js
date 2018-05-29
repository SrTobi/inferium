/*
    name: break for loop
    desc: break should jump out of for loop
 */

for(;;) {
    debug.liveCode()
    break
    debug.deadCode()
}

debug.liveCode()

for(;debug.boolean;) {
    debug.liveCode()

    for(;;) {
        debug.liveCode()
        break
        debug.deadCode()
    }

    debug.liveCode()
}


debug.liveCode()

fst: for(;;) {
    debug.liveCode()

    for(;;) {
        debug.liveCode()
        break fst
        debug.deadCode()

    }

    debug.deadCode()

}

debug.liveCode()