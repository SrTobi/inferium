/*
    name: Object merging
    desc: When writing to multiple objects, the properties should hold the correct values
 */
var a = { prop: "a" }
var b = { prop: "b" }

var o
if (debug.boolean) {
    o = a
} else {
    o = b
}

debug(a.prop).isOneOf("a")
debug(b.prop).isOneOf("b")
debug(o.prop).isOneOf("a", "b")

a.prop = "A"
debug(a.prop).isOneOf("A")
debug(b.prop).isOneOf("b")
debug(o.prop).isOneOf("A", "b")

o.prop = "O"
debug(a.prop).isOneOf("A", "O")
debug(b.prop).isOneOf("b", "O")
debug(o.prop).isOneOf("A", "b", "O")
