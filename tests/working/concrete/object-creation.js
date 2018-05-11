/*
    name: Object creation
    desc: Objects should be creatable and hold properties
 */

var o = {}
debug(o).isOneOf(o)

o.a = "a"
o.b = "b"

debug(o.a).isOneOf("a")
debug(o.b).isOneOf("b")


var o2 = { a: "a", b: "b" }
debug(o2.a).isOneOf("a")
debug(o2.b).isOneOf("b")