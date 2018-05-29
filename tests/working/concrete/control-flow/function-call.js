/*
    name: Function calls
    desc: Function calls should take parameters and return
 */

function f1() {}

debug(f1()).isOneOf(undefined)



function f2(a) { return a }

debug(f2("param")).isOneOf("param")
debug(f2()).isOneOf(undefined)



function f3(a, b, c, d) {
    return debug.squash(b, c)
}

debug(f3("a", "b", "c", "d")).isOneOf("b", "c")
debug(f3()).isOneOf(undefined)



function f4(a, b, c) {
    if (a) {
        return b
    } else {
        return c
    }
}

debug(f4(true, "then", "else")).isOneOf("then")
debug(f4(false, "then", "else")).isOneOf("else")
debug(f4(debug.boolean, "then", "else")).isOneOf("then", "else")



function f5(a) {
    if (debug.boolean) {
        return a
    }
    "nothing To do"
}

debug(f5("a")).isOneOf("a", undefined)