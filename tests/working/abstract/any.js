/*
    name: any
    desc: any should do anything to any
 */

var x = debug.any

debug(x).is(debug.any)
debug(x.x).is(debug.any)
debug(x.x.x).is(debug.any)
var a = x.x = 5
debug(a).is(5)
debug(x.x).is(debug.any)

debug(x[debug.any]).is(debug.any)
debug(x[a]).is(debug.any)
