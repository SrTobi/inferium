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

debug(test).isOneOf(t, f)
debug(test.cond).isOneOf(debug.boolean)

if (test.cond) {
    debug(test).isOneOf(t)
    debug(t.cond).isOneOf(true)
    debug(f.cond).isOneOf(false)
} else {
    debug(test).isOneOf(f)
    debug(t.cond).isOneOf(true)
    debug(f.cond).isOneOf(false)
}

debug(test).isOneOf(t, f)

if (debug.boolean) {
    test = b
}

debug(test).isOneOf(t, f, b)

if (test.cond) {
    debug(test).isOneOf(t, b)
    debug(t.cond).isOneOf(true)
    debug(f.cond).isOneOf(false)
} else {
    debug(test).isOneOf(f, b)
    debug(t.cond).isOneOf(true)
    debug(f.cond).isOneOf(false)
}