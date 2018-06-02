/*
    name: lexical scopes
    desc: Lexical read and write should respect scopes
 */

var a = "a"
a
debug.ans.is("a")

var b = "b"

debug(a).is("a")
debug(b).is("b")

debug(c).is(undefined)
c = "c"
debug(c).is("c")
var c
debug(c).is("c")

debug(d).is(undefined)
{
    var d = "d"
    debug(d).is("d")
}
debug(d).is("d")


{
    let d = "not d"
    debug(d).is("not d")

    {
        debug(d).is("not d")
        let d = "d indeed"
        debug(d).is("d indeed")
    }
    debug(d).is("not d")
}
debug(d).is("d")

var e = "e"
{
    debug(e).is("e")
    let e = "e-inner"
    debug(e).is("e-inner")
}
debug(e).is("e")