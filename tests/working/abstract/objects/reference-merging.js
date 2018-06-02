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

debug(a.prop).is("a")
debug(b.prop).is("b")
debug(o.prop).is("a", "b")

a.prop = "A"
debug(a.prop).is("A")
debug(b.prop).is("b")
debug(o.prop).is("A", "b")

o.prop = "O"
debug(a.prop).is("A", "O")
debug(b.prop).is("b", "O")
debug(o.prop).is("A", "b", "O")
