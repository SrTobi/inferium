/*
    name: filtering
    desc: Checks if conditional branching filters the condition
 */

var a = debug.boolean

if (a) {
    debug(a).is(true)
} else {
    debug(a).is(false)
}

debug(a).is(debug.boolean)


var b = debug.number

if (b) {
    debug(b).is(debug.number)
} else {
    debug(b).is(0)
}

debug(b).is(debug.number)


var c = debug.string

if (c) {
    debug(c).is(debug.string)
} else {
    debug(c).is("")
}

debug(c).is(debug.string)


var d = undefined

if (debug.boolean) {
    d = debug.number
}

if (debug.boolean) {
    d = "test"
} else {
    d = ""
}

debug(d).is(undefined, debug.number, "test", "")
if (d) {
    debug(d).is(debug.number, "test")
} else {
    debug(d).is(undefined, 0, "")
}

debug(d).is(undefined, debug.number, "test", "")