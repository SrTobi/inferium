/*
    name: filtering
    desc: Checks if conditional branching filters the condition
 */

var a = debug.boolean

if (a) {
    debug(a).isOneOf(true)
} else {
    debug(a).isOneOf(false)
}

debug(a).isOneOf(debug.boolean)


var b = debug.number

if (b) {
    debug(b).isOneOf(debug.number)
} else {
    debug(b).isOneOf(0)
}

debug(b).isOneOf(debug.number)


var c = debug.string

if (c) {
    debug(c).isOneOf(debug.string)
} else {
    debug(c).isOneOf("")
}

debug(c).isOneOf(debug.string)


var d = undefined

if (debug.boolean) {
    d = debug.number
}

if (debug.boolean) {
    d = "test"
} else {
    d = ""
}

debug(d).isOneOf(undefined, debug.number, "test", "")
if (d) {
    debug(d).isOneOf(debug.number, "test")
} else {
    debug(d).isOneOf(undefined, 0, "")
}

debug(d).isOneOf(undefined, debug.number, "test", "")