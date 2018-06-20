/*
    name: Concrete dynamic access
    desc: Dynamic access should work with concrete parameters
 */



var o = {
    a: true,
    [1]: "test",
    "2": "test2",
    "true": "test3",
    false: "test4"
}

var aname = "a"
var one = 1
var onename = "1"
var two = 2
var twoname = "2"
var t = true
var tname = "true"
var f = false
var fname = "false"

debug(o[aname]).is(true)

debug(o[one]).is("test")
debug(o[onename]).is("test")

debug(o[two]).is("test2")
debug(o[twoname]).is("test2")

debug(o[t]).is("test3")
debug(o[tname]).is("test3")

debug(o[f]).is("test4")
debug(o[fname]).is("test4")
