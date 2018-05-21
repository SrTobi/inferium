/*
    name: chain filtering
    desc: Checks if conditional branching filters chained properties
 */

var a = debug.boolean
var b = a
var c = b
var d = a

if(a) {
    debug(a).isOneOf(true)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)
    debug(d).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(false)
    debug(c).isOneOf(false)
    debug(d).isOneOf(false)
}


if(c) {
    debug(a).isOneOf(true)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)
    debug(d).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(false)
    debug(c).isOneOf(false)
    debug(d).isOneOf(false)
}

if(d) {
    debug(a).isOneOf(true)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)
    debug(d).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(false)
    debug(c).isOneOf(false)
    debug(d).isOneOf(false)
}
