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

debug(a).isOneOf("in finally")


let b = "init"

try {
    b = "in try"
    try {
        debug(b).isOneOf("in try")
        b = "in inner try"
    } finally {
        debug(b).isOneOf("in inner try")
        b = "in inner finally"
    }
} finally {
    debug(b).isOneOf("in inner finally")
    b = "in finally"
}

debug(b).isOneOf("in finally")
