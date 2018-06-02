/*
    name: Property Semantics
    desc: properties should be flow- and instance-sensitive
 */

var o = {}


// check overwrite
o.prop = "prop"
debug(o.prop).is("prop")

o.prop = "something else"
debug(o.prop).is("something else")

{
    const a = o
    a.prop = "written by a"
}
debug(o.prop).is("written by a")