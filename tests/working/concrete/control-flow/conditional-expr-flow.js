/*
    name: flow through conditional expr
    desc: conditional expressions should execute correctly
 */

var t = true
var f = false
var b = debug.boolean


var res1 = t? "then" : debug("never").deadCode()
debug(res1).isOneOf("then")

var res2 = f? debug("never").deadCode() : "else"
debug(res2).isOneOf("else")

var res3 = b? "then" : "else"
debug(res3).isOneOf("then", "else")