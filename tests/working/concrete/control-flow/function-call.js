/*
    name: Function calls
    desc: Function calls should take parameters and return
 */

function f1() {}

debug(f1()).is(undefined)



function f2(a) { return a }

debug(f2("param")).is("param")
debug(f2()).is(undefined)



function f3(a, b, c, d) {
    return debug.squash(b, c)
}

debug(f3("a", "b", "c", "d")).is("b", "c")
debug(f3()).is(undefined)



function f4(a, b, c) {
    if (a) {
        return b
    } else {
        return c
    }
}

debug(f4(true, "then", "else")).is("then")
debug(f4(false, "then", "else")).is("else")
debug(f4(debug.boolean, "then", "else")).is("then", "else")



function f5(a) {
    if (debug.boolean) {
        return a
    }
    "nothing To do"
}

debug(f5("a")).is("a", undefined)