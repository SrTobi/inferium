/*
    name: Function declaration
    desc: functions should be hoisted and callable
 */

function f1() {}

debug(f1()).is(undefined)


function f2() {
    debug.liveCode()
    return "return"
}

debug(f2()).is("return")

{
    function f2() {
        debug.liveCode()
        return "inner"
    }

    debug(f2()).is("inner")
}

debug(f2()).is("inner")

debug(f3()).is("return2")

function f3() {
    debug.liveCode()
    return "return2"
}

debug(f3()).is("return2")

{
    debug(f3()).is("inner2")

    function f3() {
        debug.liveCode()
        return "inner2"
    }
    debug(f3()).is("inner2")
}


debug(f3()).is("inner2")