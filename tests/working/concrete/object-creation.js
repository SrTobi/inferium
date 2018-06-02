/*
    name: Object creation
    desc: Objects should be creatable and hold properties
 */

var o = {}
debug(o).is(o)

o.a = "a"
o.b = "b"

debug(o.a).is("a")
debug(o.b).is("b")


var o2 = { a: "a", b: "b" }
debug(o2.a).is("a")
debug(o2.b).is("b")