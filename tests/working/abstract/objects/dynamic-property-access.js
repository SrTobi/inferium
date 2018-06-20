/*
    name: dynamic property access
    desc: write/read to unknown properties
 */


var o = { p: "init" }

debug(o.p).is("init")

o[debug.string] = "haha"

debug(o.p).is("haha", "init")
debug(o[debug.string]).is("haha", "init", undefined)

o.p = "reset"

debug(o.p).is("reset")
debug(o[debug.string]).is("haha", "reset", undefined)



var o2 = { [42]: "init", p: "string" }

debug(o2.p).is("string")
debug(o2[42]).is("init")
debug(o2[debug.squash(42, "p")]).is("init", "string")
debug(o2[debug.number]).is("init", undefined)
debug(o2[debug.string]).is("string", "init", undefined)

o2[debug.number] = "haha"

debug(o2.p).is("string")
debug(o2[42]).is("haha", "init")
debug(o2[debug.number]).is("haha", "init", undefined)
debug(o2[debug.string]).is("string", "haha", "init", undefined)