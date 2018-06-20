/*
    name: Constant dynamic access
    desc: Dynamic access should work with constant parameters
 */



var o = {
    a: true,
    [1]: "test",
    "2": "test2",
    "true": "test3",
    false: "test4",
    "undefined": "test5"
}

debug(o.a).is(true)
debug(o["a"]).is(true)

debug(o[1]).is("test")
debug(o["1"]).is("test")

debug(o["2"]).is("test2")
debug(o[2]).is("test2")

debug(o["true"]).is("test3")
debug(o[true]).is("test3")

debug(o[false]).is("test4")
debug(o["false"]).is("test4")

debug(o[undefined]).is("test5")
debug(o["undefined"]).is("test5")
