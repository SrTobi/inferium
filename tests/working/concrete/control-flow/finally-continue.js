/*
    name: continue with finally
    desc: finally code should be executed even if continued out of a try block
 */

let a = "init"
while (debug.boolean) {
    debug(a).is("init", "in finally")
    try {
        a = "in try"
        continue
        debug.deadCode()
    } finally {
        debug(a).is("in try").mightBeDead()
        a = "in finally"
    }
    debug.deadCode()
}

debug(a).is("init", "in finally")