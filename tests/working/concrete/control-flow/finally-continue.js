/*
    name: continue with finally
    desc: finally code should be executed even if continued out of a try block
 */

let a = "init"
while (debug.boolean) {
    debug(a).isOneOf("init", "in finally")
    try {
        a = "in try"
        continue
        debug.deadCode()
    } finally {
        debug(a).isOneOf("in try").mightBeDead()
        a = "in finally"
    }
    debug.deadCode()
}

debug(a).isOneOf("init", "in finally")