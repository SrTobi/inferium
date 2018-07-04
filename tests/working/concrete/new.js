/*
    name: new
    desc: new creates a new function and calls the constructor
 */

function T1(p) {
    this.x = "init"
    this.y = p
}

var p = T1.prototype
p.base = "base"

var t1 = new T1("t1")
var t2 = new T1("t2")
debug(t1.x).is("init")
debug(t1.y).is("t1")

debug(t2.x).is("init")
debug(t2.y).is("t2")

debug(t1.base).is("base")
debug(t2.base).is("base")

T1.prototype = {}

debug(t1.base).is("base")
debug(t2.base).is("base")

var t3 = new T1("xxx")
debug(t3.base).is(undefined)

function T2() {
    this.x = "test"
    return {x: "haha" }
}

var t4 = new T2
debug(t4.x).is("haha")