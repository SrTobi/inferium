/*
    name: lexical scopes
    desc: Lexical read and write should respect scopes
 */

var a = "a"
a
debug.ans.isOneOf("a")

var b = "b"

debug(a).isOneOf("a")
debug(b).isOneOf("b")

debug(c).isOneOf(undefined)
c = "c"
debug(c).isOneOf("c")
var c
debug(c).isOneOf("c")

debug(d).isOneOf(undefined)
{
    var d = "d"
    debug(d).isOneOf("d")
}
debug(d).isOneOf("d")


{
    let d = "not d"
    debug(d).isOneOf("not d")

    {
        debug(d).isOneOf("not d")
        let d = "d indeed"
        debug(d).isOneOf("d indeed")
    }
    debug(d).isOneOf("not d")
}
debug(d).isOneOf("d")

var e = "e"
{
    debug(e).isOneOf("e")
    let e = "e-inner"
    debug(e).isOneOf("e-inner")
}
debug(e).isOneOf("e")