/*
    name: fan filtering
    desc: Checks if conditional branching filters properties respecting union values
 */

var a = false
var b = true
var c = true

var d = debug.squash(a, b)
var e = debug.squash(c, d)

debug(a).isOneOf(false)
debug(b).isOneOf(true)
debug(c).isOneOf(true)

debug(d).isOneOf(debug.boolean)
debug(e).isOneOf(debug.boolean)

if (d) {
    debug(a).isOneOf(false)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(true)
    debug(e).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(false)
    debug(e).isOneOf(debug.boolean)
}

if (e) {
    debug(a).isOneOf(false)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(debug.boolean)
    debug(e).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(false)
    debug(e).isOneOf(false)
}

a = true

if (e) {
    debug(a).isOneOf(true)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(debug.boolean)
    debug(e).isOneOf(true)
} else {
    debug(a).isOneOf(false)
    debug(b).isOneOf(true)
    debug(c).isOneOf(true)

    debug(d).isOneOf(false)
    debug(e).isOneOf(false)
}

