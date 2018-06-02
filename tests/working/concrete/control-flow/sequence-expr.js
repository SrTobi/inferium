/*
    name: sequence expr
    desc: Sequence expression should execute all expressions in its sequence
 */

var a = (1, 2)
debug(a).is(2)

var b = ("test", 4, "last")
debug(b).is("last")

if (true, false) {
    debug.deadCode()
} else {
    debug.liveCode()
}