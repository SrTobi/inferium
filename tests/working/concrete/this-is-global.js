/*
    name: this is global
    desc: this should point to the global object outside of functions
 */

var g = global
var ths = this

debug(ths).isOneOf(g)