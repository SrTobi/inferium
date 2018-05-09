/*
    name: Abstract instances
    desc: Object instances should become abstract in iteration
 */

var lastObj
var someObj
while (debug.boolean) {
    lastObj = { prop: "init" }
    if (debug.boolean) {
        someObj = lastObj
    }

    debug(lastObj.prop).isOneOf("init")
    lastObj.prop = "next"
    debug(lastObj.prop).isOneOf("next")


    if (debug.boolean) {
        someObj.prop = "blub"
    }
}

debug(lastObj.prop).isOneOf("next", "blub")
debug(someObj.prop).isOneOf("next", "blub")