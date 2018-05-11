/*
    name: Object merging
    desc: When objects get merged, properties should merge as well
 */
var o = {}

if (debug.boolean) {
    o.prop = "yes"
} else {
    o.prop = "no"
}

debug(o.prop).isOneOf("yes", "no")

o.prop = "clear"

if (debug.boolean) {
    o.prop = "then"
}

debug(o.prop).isOneOf("clear", "then")


o.prop = "clear"
if (debug.boolean) {

} else {
    o.prop = "else"
}

debug(o.prop).isOneOf("clear", "else")
