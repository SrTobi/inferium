/*
    name: simple finally
    desc: Finally should be executed if try block exists normally
 */

let a = "init"

try {
    a = "in try"
} finally {
    a = "in finally"
}

debug(a).is("in finally")


let b = "init"

try {
    b = "in try"
    try {
        debug(b).is("in try")
        b = "in inner try"
    } finally {
        debug(b).is("in inner try")
        b = "in inner finally"
    }
} finally {
    debug(b).is("in inner finally")
    b = "in finally"
}

debug(b).is("in finally")
