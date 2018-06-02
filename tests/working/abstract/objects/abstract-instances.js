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

    debug(lastObj.prop).is("init")
    lastObj.prop = "next"
    debug(lastObj.prop).is("next")


    if (debug.boolean) {
        someObj.prop = "blub"
    }
}

debug(lastObj.prop).is("next", "blub")
debug(someObj.prop).is("next", "blub")

someObj.absProp = "abs"
debug(someObj.absProp).is("abs", undefined)