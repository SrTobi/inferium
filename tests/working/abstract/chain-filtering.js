/*
    name: chain filtering
    desc: Checks if conditional branching filters chained properties
 */

var a = debug.boolean
var b = a
var c = b
var d = a

if(a) {
    debug(a).is(true)
    debug(b).is(true)
    debug(c).is(true)
    debug(d).is(true)
} else {
    debug(a).is(false)
    debug(b).is(false)
    debug(c).is(false)
    debug(d).is(false)
}


if(c) {
    debug(a).is(true)
    debug(b).is(true)
    debug(c).is(true)
    debug(d).is(true)
} else {
    debug(a).is(false)
    debug(b).is(false)
    debug(c).is(false)
    debug(d).is(false)
}

if(d) {
    debug(a).is(true)
    debug(b).is(true)
    debug(c).is(true)
    debug(d).is(true)
} else {
    debug(a).is(false)
    debug(b).is(false)
    debug(c).is(false)
    debug(d).is(false)
}
