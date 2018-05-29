/*
    name: Function declaration
    desc: functions should be hoisted and callable
 */

function f1() {}

debug(f1()).isOneOf(undefined)


function f2() {
    debug.liveCode()
    return "return"
}

debug(f2()).isOneOf("return")

{
    function f2() {
        debug.liveCode()
        return "inner"
    }

    debug(f2()).isOneOf("inner")
}

debug(f2()).isOneOf("inner")

debug(f3()).isOneOf("return2")

function f3() {
    debug.liveCode()
    return "return2"
}

debug(f3()).isOneOf("return2")

{
    debug(f3()).isOneOf("inner2")

    function f3() {
        debug.liveCode()
        return "inner2"
    }
    debug(f3()).isOneOf("inner2")
}


debug(f3()).isOneOf("inner2")