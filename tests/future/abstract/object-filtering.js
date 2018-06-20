/*
    name: Object filtering
    desc: Checks if conditional branching filters the base object of references
 */

var t = { cond: true }
var f = { cond: false }
var b = { cond: debug.boolean }

if (debug.boolean) {
    var test = t
} else {
    test = f
}

debug(test).is(t, f)
debug(test.cond).is(debug.boolean)

if (test.cond) {
    debug(test).is(t)
    debug(t.cond).is(true)
    debug(f.cond).is(false)
} else {
    debug(test).is(f)
    debug(t.cond).is(true)
    debug(f.cond).is(false)
}

debug(test).is(t, f)

if (debug.boolean) {
    test = b
}

debug(test).is(t, f, b)

if (test.cond) {
    debug(test).is(t, b)
    debug(t.cond).is(true)
    debug(f.cond).is(false)
} else {
    debug(test).is(f, b)
    debug(t.cond).is(true)
    debug(f.cond).is(false)
}