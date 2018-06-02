/*
    name: fan filtering
    desc: Checks if conditional branching filters properties respecting union values
 */

var a = false
var b = true
var c = true

var d = debug.squash(a, b)
var e = debug.squash(c, d)

debug(a).is(false)
debug(b).is(true)
debug(c).is(true)

debug(d).is(debug.boolean)
debug(e).is(debug.boolean)

if (d) {
    debug(a).is(false)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(true)
    debug(e).is(true)
} else {
    debug(a).is(false)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(false)
    debug(e).is(debug.boolean)
}

if (e) {
    debug(a).is(false)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(debug.boolean)
    debug(e).is(true)
} else {
    debug(a).is(false)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(false)
    debug(e).is(false)
}

a = true

if (e) {
    debug(a).is(true)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(debug.boolean)
    debug(e).is(true)
} else {
    debug(a).is(true)
    debug(b).is(true)
    debug(c).is(true)

    debug(d).is(false)
    debug(e).is(false)
}

