/*
    name: break finally
    desc: finally code should be executed even if break out of the try block
 */

let a = "init"

fst: try {
    a = "in try"
    break fst
    debug.deadCode()
} finally {
    debug(a).is("in try").mightBeDead()
    a = "in finally"
}

debug(a).is("in finally")


let b = "init"

snd: try {
    b = "in try"
    try {
        b = "in inner try"
        break snd
        debug.deadCode()
    } finally {
        debug(b).is("in inner try").mightBeDead()
        b = "in inner finally"
    }
    debug.deadCode()
} finally {
    debug(b).is("in inner finally").mightBeDead()
    b = "in finally"
}

debug(b).is("in finally")
