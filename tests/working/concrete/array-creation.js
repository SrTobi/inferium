/*
    name: array creation
    desc: Arrays can be created and have their elements set
 */

var a = [1, "test", , "test", "blub",]

debug(a[0]).is(1)
debug(a[1]).is("test")
debug(a[2]).is(undefined)
debug(a[3]).is("test")
debug(a[4]).is("blub")
debug(a.length).is(5)

debug(a[debug.number]).is(undefined, 1, "test", "blub")