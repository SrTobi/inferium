
package inferium.dataflow

import inferium.debug.{Fixture, FixtureRunner}
import org.scalatest.{FreeSpec, Matchers}

class WorkingFixturesSpec extends FreeSpec with Matchers {
            
    "/abstract" - {
        "any should do anything to any" in {
            val code =
                """/*
                  |    name: any
                  |    desc: any should do anything to any
                  | */
                  |
                  |var x = debug.any
                  |
                  |debug(x).is(debug.any)
                  |debug(x.x).is(debug.any)
                  |debug(x.x.x).is(debug.any)
                  |var a = x.x = 5
                  |debug(a).is(5)
                  |debug(x.x).is(debug.any)
                  |
                  |debug(x[debug.any]).is(debug.any)
                  |debug(x[a]).is(debug.any)
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


    "/abstract/objects" - {
        "Object instances should become abstract in iteration" in {
            val code =
                """/*
                  |    name: Abstract instances
                  |    desc: Object instances should become abstract in iteration
                  | */
                  |
                  |var lastObj
                  |var someObj
                  |while (debug.boolean) {
                  |    lastObj = { prop: "init" }
                  |    if (debug.boolean) {
                  |        someObj = lastObj
                  |    }
                  |
                  |    debug(lastObj.prop).is("init")
                  |    lastObj.prop = "next"
                  |    debug(lastObj.prop).is("next")
                  |
                  |
                  |    if (debug.boolean) {
                  |        someObj.prop = "blub"
                  |    }
                  |}
                  |
                  |debug(lastObj.prop).is("next", "blub")
                  |debug(someObj.prop).is("next", "blub")
                  |
                  |someObj.absProp = "abs"
                  |debug(someObj.absProp).is("abs", undefined)
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "write/read to unknown properties" in {
            val code =
                """/*
                  |    name: dynamic property access
                  |    desc: write/read to unknown properties
                  | */
                  |
                  |
                  |var o = { p: "init" }
                  |
                  |debug(o.p).is("init")
                  |
                  |o[debug.string] = "haha"
                  |
                  |debug(o.p).is("haha", "init")
                  |debug(o[debug.string]).is("haha", "init", undefined)
                  |
                  |o.p = "reset"
                  |
                  |debug(o.p).is("reset")
                  |debug(o[debug.string]).is("haha", "reset", undefined)
                  |
                  |
                  |
                  |var o2 = { [42]: "init", p: "string" }
                  |
                  |debug(o2.p).is("string")
                  |debug(o2[42]).is("init")
                  |debug(o2[debug.squash(42, "p")]).is("init", "string")
                  |debug(o2[debug.number]).is("init", undefined)
                  |debug(o2[debug.string]).is("string", "init", undefined)
                  |
                  |o2[debug.number] = "haha"
                  |
                  |debug(o2.p).is("string")
                  |debug(o2[42]).is("haha", "init")
                  |debug(o2[debug.number]).is("haha", "init", undefined)
                  |debug(o2[debug.string]).is("string", "haha", "init", undefined)
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "When objects get merged, properties should merge as well" in {
            val code =
                """/*
                  |    name: Object merging
                  |    desc: When objects get merged, properties should merge as well
                  | */
                  |var o = {}
                  |
                  |if (debug.boolean) {
                  |    o.prop = "yes"
                  |} else {
                  |    o.prop = "no"
                  |}
                  |
                  |debug(o.prop).is("yes", "no")
                  |
                  |o.prop = "clear"
                  |
                  |if (debug.boolean) {
                  |    o.prop = "then"
                  |}
                  |
                  |debug(o.prop).is("clear", "then")
                  |
                  |
                  |o.prop = "clear"
                  |if (debug.boolean) {
                  |
                  |} else {
                  |    o.prop = "else"
                  |}
                  |
                  |debug(o.prop).is("clear", "else")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "When writing to multiple objects, the properties should hold the correct values" in {
            val code =
                """/*
                  |    name: Object merging
                  |    desc: When writing to multiple objects, the properties should hold the correct values
                  | */
                  |var a = { prop: "a" }
                  |var b = { prop: "b" }
                  |
                  |var o
                  |if (debug.boolean) {
                  |    o = a
                  |} else {
                  |    o = b
                  |}
                  |
                  |debug(a.prop).is("a")
                  |debug(b.prop).is("b")
                  |debug(o.prop).is("a", "b")
                  |
                  |a.prop = "A"
                  |debug(a.prop).is("A")
                  |debug(b.prop).is("b")
                  |debug(o.prop).is("A", "b")
                  |
                  |o.prop = "O"
                  |debug(a.prop).is("A", "O")
                  |debug(b.prop).is("b", "O")
                  |debug(o.prop).is("A", "b", "O")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


    "/concrete" - {
        "After a statement, ans should have the correct value" in {
            val code =
                """/*
                  |    name: ans from statement
                  |    desc: After a statement, ans should have the correct value
                  | */
                  |
                  |debug.ans.is(undefined);
                  |
                  |"a"
                  |{
                  |    // nothing
                  |}
                  |debug.ans.is("a");
                  |
                  |
                  |"b"
                  |{
                  |    "c"
                  |}
                  |debug.ans.is("c");
                  |
                  |
                  |"d"
                  |try {
                  |    "e"
                  |} catch (e) {
                  |    "f"
                  |}
                  |debug.ans.is("e");
                  |
                  |
                  |"g"
                  |try {
                  |    "h"
                  |} finally {
                  |    "i"
                  |}
                  |debug.ans.is("i");
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "After an expression statement, ans should be the result of the expression" in {
            val code =
                """/*
                  |    name: ans from Expression statement
                  |    desc: After an expression statement, ans should be the result of the expression
                  | */
                  |
                  |// when the program is started ans should be undefined
                  |debug.ans.is(undefined);
                  |
                  |undefined;
                  |debug.ans.is(undefined);
                  |
                  |null;
                  |debug.ans.is(null);
                  |
                  |0;
                  |debug.ans.is(0);
                  |
                  |//(-8);
                  |//debug.ans.is(-8);
                  |//debug.ans.is(debug.number);
                  |
                  |"test";
                  |debug.ans.is("test");
                  |
                  |"";
                  |debug.ans.is("");
                  |
                  |true;
                  |debug.ans.is(true);
                  |
                  |false;
                  |debug.ans.is(false);
                  |
                  |
                  |// test the debug helpers
                  |debug.boolean;
                  |debug.ans.is(debug.boolean);
                  |
                  |debug.number;
                  |debug.ans.is(debug.number);
                  |
                  |debug.string;
                  |debug.ans.is(debug.string);
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Dynamic access should work with concrete parameters" in {
            val code =
                """/*
                  |    name: Concrete dynamic access
                  |    desc: Dynamic access should work with concrete parameters
                  | */
                  |
                  |
                  |
                  |var o = {
                  |    a: true,
                  |    [1]: "test",
                  |    "2": "test2",
                  |    "true": "test3",
                  |    false: "test4"
                  |}
                  |
                  |var aname = "a"
                  |var one = 1
                  |var onename = "1"
                  |var two = 2
                  |var twoname = "2"
                  |var t = true
                  |var tname = "true"
                  |var f = false
                  |var fname = "false"
                  |
                  |debug(o[aname]).is(true)
                  |
                  |debug(o[one]).is("test")
                  |debug(o[onename]).is("test")
                  |
                  |debug(o[two]).is("test2")
                  |debug(o[twoname]).is("test2")
                  |
                  |debug(o[t]).is("test3")
                  |debug(o[tname]).is("test3")
                  |
                  |debug(o[f]).is("test4")
                  |debug(o[fname]).is("test4")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Dynamic access should work with constant parameters" in {
            val code =
                """/*
                  |    name: Constant dynamic access
                  |    desc: Dynamic access should work with constant parameters
                  | */
                  |
                  |
                  |
                  |var o = {
                  |    a: true,
                  |    [1]: "test",
                  |    "2": "test2",
                  |    "true": "test3",
                  |    false: "test4",
                  |    "undefined": "test5"
                  |}
                  |
                  |debug(o.a).is(true)
                  |debug(o["a"]).is(true)
                  |
                  |debug(o[1]).is("test")
                  |debug(o["1"]).is("test")
                  |
                  |debug(o["2"]).is("test2")
                  |debug(o[2]).is("test2")
                  |
                  |debug(o["true"]).is("test3")
                  |debug(o[true]).is("test3")
                  |
                  |debug(o[false]).is("test4")
                  |debug(o["false"]).is("test4")
                  |
                  |debug(o[undefined]).is("test5")
                  |debug(o["undefined"]).is("test5")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "A directive should also influence the ans value" in {
            val code =
                """/*
                  |    name: Directive is a string-expression
                  |    desc: A directive should also influence the ans value
                  | */
                  |
                  |"use strict";
                  |
                  |debug.ans.is("use strict")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Empty script should be parsable" in {
            val code =
                """/*
                  |    name: empty
                  |    desc: Empty script should be parsable
                  | */
                  |
                  |// nothing to do
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Lexical read and write should respect scopes" in {
            val code =
                """/*
                  |    name: lexical scopes
                  |    desc: Lexical read and write should respect scopes
                  | */
                  |
                  |var a = "a"
                  |a
                  |debug.ans.is("a")
                  |
                  |var b = "b"
                  |
                  |debug(a).is("a")
                  |debug(b).is("b")
                  |
                  |debug(c).is(undefined)
                  |c = "c"
                  |debug(c).is("c")
                  |var c
                  |debug(c).is("c")
                  |
                  |debug(d).is(undefined)
                  |{
                  |    var d = "d"
                  |    debug(d).is("d")
                  |}
                  |debug(d).is("d")
                  |
                  |
                  |{
                  |    let d = "not d"
                  |    debug(d).is("not d")
                  |
                  |    {
                  |        debug(d).is("not d")
                  |        let d = "d indeed"
                  |        debug(d).is("d indeed")
                  |    }
                  |    debug(d).is("not d")
                  |}
                  |debug(d).is("d")
                  |
                  |var e = "e"
                  |{
                  |    debug(e).is("e")
                  |    let e = "e-inner"
                  |    debug(e).is("e-inner")
                  |}
                  |debug(e).is("e")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Objects should be creatable and hold properties" in {
            val code =
                """/*
                  |    name: Object creation
                  |    desc: Objects should be creatable and hold properties
                  | */
                  |
                  |var o = {}
                  |debug(o).is(o)
                  |
                  |o.a = "a"
                  |o.b = "b"
                  |
                  |debug(o.a).is("a")
                  |debug(o.b).is("b")
                  |
                  |
                  |var o2 = { a: "a", b: "b" }
                  |debug(o2.a).is("a")
                  |debug(o2.b).is("b")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "properties should be flow- and instance-sensitive" in {
            val code =
                """/*
                  |    name: Property Semantics
                  |    desc: properties should be flow- and instance-sensitive
                  | */
                  |
                  |var o = {}
                  |
                  |
                  |// check overwrite
                  |o.prop = "prop"
                  |debug(o.prop).is("prop")
                  |
                  |o.prop = "something else"
                  |debug(o.prop).is("something else")
                  |
                  |{
                  |    const a = o
                  |    a.prop = "written by a"
                  |}
                  |debug(o.prop).is("written by a")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "this should point to the global object outside of functions" in {
            val code =
                """/*
                  |    name: this is global
                  |    desc: this should point to the global object outside of functions
                  | */
                  |
                  |var g = global
                  |var ths = this
                  |
                  |debug(ths).is(g)
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


    "/concrete/control-flow" - {
        "break should jump out of do-while loop" in {
            val code =
                """/*
                  |    name: break do-while loop
                  |    desc: break should jump out of do-while loop
                  | */
                  |
                  |do {
                  |    debug.liveCode()
                  |    break
                  |    debug.deadCode()
                  |} while (debug(debug.boolean).deadCode())
                  |
                  |debug.liveCode()
                  |
                  |fst: do {
                  |    debug.liveCode()
                  |    do {
                  |        debug.liveCode()
                  |        break fst
                  |        debug.deadCode()
                  |
                  |    } while (debug(debug.boolean).deadCode())
                  |
                  |    debug.deadCode()
                  |
                  |} while (debug(debug.boolean).deadCode())
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "break should jump out of for loop" in {
            val code =
                """/*
                  |    name: break for loop
                  |    desc: break should jump out of for loop
                  | */
                  |
                  |for(;;) {
                  |    debug.liveCode()
                  |    break
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |for(;debug.boolean;) {
                  |    debug.liveCode()
                  |
                  |    for(;;) {
                  |        debug.liveCode()
                  |        break
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug.liveCode()
                  |}
                  |
                  |
                  |debug.liveCode()
                  |
                  |fst: for(;;) {
                  |    debug.liveCode()
                  |
                  |    for(;;) {
                  |        debug.liveCode()
                  |        break fst
                  |        debug.deadCode()
                  |
                  |    }
                  |
                  |    debug.deadCode()
                  |
                  |}
                  |
                  |debug.liveCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "break should jump to referenced label" in {
            val code =
                """/*
                  |    name: syntactic break
                  |    desc: break should jump to referenced label
                  | */
                  |
                  |fst: {
                  |    debug.liveCode()
                  |    break fst
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |snd: {
                  |    inner: {
                  |        debug.liveCode()
                  |        break snd
                  |        debug.deadCode()
                  |    }
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |outer: {
                  |    third: {
                  |        debug.liveCode()
                  |        break third
                  |        debug.deadCode()
                  |    }
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |forth:
                  |    break forth;
                  |
                  |debug.liveCode()
                  |
                  |fifth:
                  |after: {
                  |    debug.liveCode()
                  |    break fifth
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "break should break the current loop" in {
            val code =
                """/*
                  |    name: loop break
                  |    desc: break should break the current loop
                  | */
                  |
                  |
                  |while (true) {
                  |    debug.liveCode()
                  |    break
                  |    debug.deadCode()
                  |}
                  |
                  |
                  |while (debug.boolean) {
                  |    debug.liveCode()
                  |
                  |    while (true) {
                  |        debug.liveCode()
                  |        break
                  |        debug.deadCode()
                  |    }
                  |    debug.liveCode()
                  |}
                  |
                  |
                  |debug.liveCode()
                  |
                  |fst: while(true) {
                  |    debug.liveCode()
                  |    break fst
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |snd: while(true) {
                  |    debug.liveCode()
                  |    inner: while(true) {
                  |        debug.liveCode()
                  |        break snd
                  |        debug.deadCode()
                  |    }
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "conditional expressions should execute correctly" in {
            val code =
                """/*
                  |    name: flow through conditional expr
                  |    desc: conditional expressions should execute correctly
                  | */
                  |
                  |var t = true
                  |var f = false
                  |var b = debug.boolean
                  |
                  |
                  |var res1 = t? "then" : debug("never").deadCode()
                  |debug(res1).is("then")
                  |
                  |var res2 = f? debug("never").deadCode() : "else"
                  |debug(res2).is("else")
                  |
                  |var res3 = b? "then" : "else"
                  |debug(res3).is("then", "else")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "continue should restart do-while loop" in {
            val code =
                """/*
                  |    name: continue do-while
                  |    desc: continue should restart do-while loop
                  | */
                  |
                  |do {
                  |    debug.liveCode()
                  |    continue
                  |    debug.deadCode()
                  |
                  |} while (debug(debug.boolean).liveCode())
                  |
                  |debug.liveCode()
                  |
                  |fst: do {
                  |
                  |    debug.liveCode()
                  |
                  |    do {
                  |        debug.liveCode()
                  |        continue fst
                  |        debug.deadCode()
                  |    } while (debug(true).deadCode())
                  |
                  |    debug.deadCode()
                  |
                  |} while (debug(debug.boolean).liveCode())
                  |
                  |debug.liveCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "continue restarts the most inner or labeled for-loop" in {
            val code =
                """/*
                  |    name: for-loop continue
                  |    desc: continue restarts the most inner or labeled for-loop
                  | */
                  |
                  |for (let a = true; debug.boolean; a = false) {
                  |    debug(a).is(debug.boolean)
                  |    continue
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |for (let a = "init"; debug.boolean; debug(a).is("changed")) {
                  |    debug(a).is("init", "changed")
                  |    a = "changed"
                  |    continue
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |for (let b = "init"; debug.boolean; b = "update") {
                  |    debug(b).is("init", "update")
                  |
                  |    for (let b = true; debug.boolean; b = false) {
                  |        debug(b).is(debug.boolean)
                  |        continue
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |let c = "init"
                  |fst: for (debug(c).is("init"); debug.boolean; c = "update") {
                  |    debug(c).is("init", "update")
                  |    continue fst
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |let d = "init"
                  |snd: for (debug(d).is("init"); debug.boolean; d = "update") {
                  |    debug(d).is("init", "update")
                  |
                  |    inner: for (d = "init2" ;debug.boolean; debug("never").deadCode()) {
                  |        debug(d).is("init2")
                  |        d = "changed"
                  |        continue snd
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug(d).is("init2")
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "continue starts the most inner or labeled loop over" in {
            val code =
                """/*
                  |    name: loop continue
                  |    desc: continue starts the most inner or labeled loop over
                  | */
                  |
                  |while (debug.boolean) {
                  |    debug.liveCode()
                  |    continue
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |while (debug.boolean) {
                  |    debug.liveCode()
                  |
                  |    while (debug.boolean) {
                  |        debug.liveCode()
                  |        continue
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |
                  |fst: while (debug.boolean) {
                  |    debug.liveCode()
                  |    continue fst
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |snd: while (debug.boolean) {
                  |    debug.liveCode()
                  |
                  |    var a = "init"
                  |
                  |    inner: while (debug.boolean) {
                  |        debug.liveCode()
                  |        debug(a).is("init")
                  |        a = "changed"
                  |        continue snd
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug(a).is("init")
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |third: while (debug.boolean) {
                  |    debug.liveCode()
                  |
                  |    debug(b).is(undefined, "init", "changed")
                  |    var b = "init"
                  |
                  |    inner: while (debug.boolean) {
                  |        debug.liveCode()
                  |        debug(b).is("init")
                  |        b = "changed"
                  |        continue third
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug(b).is("init")
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |while (true) {
                  |    continue
                  |}
                  |
                  |debug.deadCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Do-while loops should work correctly" in {
            val code =
                """/*
                  |    name: do-while loop
                  |    desc: Do-while loops should work correctly
                  | */
                  |
                  |do {
                  |    debug.liveCode()
                  |} while (false)
                  |
                  |debug.liveCode()
                  |
                  |do {
                  |    debug.liveCode()
                  |} while (true)
                  |
                  |debug.deadCode()
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "finally code should be executed even if break out of the try block" in {
            val code =
                """/*
                  |    name: break finally
                  |    desc: finally code should be executed even if break out of the try block
                  | */
                  |
                  |let a = "init"
                  |
                  |fst: try {
                  |    a = "in try"
                  |    break fst
                  |    debug.deadCode()
                  |} finally {
                  |    debug(a).is("in try").mightBeDead()
                  |    a = "in finally"
                  |}
                  |
                  |debug(a).is("in finally")
                  |
                  |
                  |let b = "init"
                  |
                  |snd: try {
                  |    b = "in try"
                  |    try {
                  |        b = "in inner try"
                  |        break snd
                  |        debug.deadCode()
                  |    } finally {
                  |        debug(b).is("in inner try").mightBeDead()
                  |        b = "in inner finally"
                  |    }
                  |    debug.deadCode()
                  |} finally {
                  |    debug(b).is("in inner finally").mightBeDead()
                  |    b = "in finally"
                  |}
                  |
                  |debug(b).is("in finally")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "finally code should be executed even if continued out of a try block" in {
            val code =
                """/*
                  |    name: continue with finally
                  |    desc: finally code should be executed even if continued out of a try block
                  | */
                  |
                  |let a = "init"
                  |while (debug.boolean) {
                  |    debug(a).is("init", "in finally")
                  |    try {
                  |        a = "in try"
                  |        continue
                  |        debug.deadCode()
                  |    } finally {
                  |        debug(a).is("in try").mightBeDead()
                  |        a = "in finally"
                  |    }
                  |    debug.deadCode()
                  |}
                  |
                  |debug(a).is("init", "in finally")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Finally should be executed if try block exists normally" in {
            val code =
                """/*
                  |    name: simple finally
                  |    desc: Finally should be executed if try block exists normally
                  | */
                  |
                  |let a = "init"
                  |
                  |try {
                  |    a = "in try"
                  |} finally {
                  |    a = "in finally"
                  |}
                  |
                  |debug(a).is("in finally")
                  |
                  |
                  |let b = "init"
                  |
                  |try {
                  |    b = "in try"
                  |    try {
                  |        debug(b).is("in try")
                  |        b = "in inner try"
                  |    } finally {
                  |        debug(b).is("in inner try")
                  |        b = "in inner finally"
                  |    }
                  |} finally {
                  |    debug(b).is("in inner finally")
                  |    b = "in finally"
                  |}
                  |
                  |debug(b).is("in finally")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Function calls should take parameters and return" in {
            val code =
                """/*
                  |    name: Function calls
                  |    desc: Function calls should take parameters and return
                  | */
                  |
                  |function f1() {}
                  |
                  |debug(f1()).is(undefined)
                  |
                  |
                  |
                  |function f2(a) { return a }
                  |
                  |debug(f2("param")).is("param")
                  |debug(f2()).is(undefined)
                  |
                  |
                  |
                  |function f3(a, b, c, d) {
                  |    return debug.squash(b, c)
                  |}
                  |
                  |debug(f3("a", "b", "c", "d")).is("b", "c")
                  |debug(f3()).is(undefined)
                  |
                  |
                  |
                  |function f4(a, b, c) {
                  |    if (a) {
                  |        return b
                  |    } else {
                  |        return c
                  |    }
                  |}
                  |
                  |debug(f4(true, "then", "else")).is("then")
                  |debug(f4(false, "then", "else")).is("else")
                  |debug(f4(debug.boolean, "then", "else")).is("then", "else")
                  |
                  |
                  |
                  |function f5(a) {
                  |    if (debug.boolean) {
                  |        return a
                  |    }
                  |    "nothing To do"
                  |}
                  |
                  |debug(f5("a")).is("a", undefined)
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "functions should be hoisted and callable" in {
            val code =
                """/*
                  |    name: Function declaration
                  |    desc: functions should be hoisted and callable
                  | */
                  |
                  |function f1() {}
                  |
                  |debug(f1()).is(undefined)
                  |
                  |
                  |function f2() {
                  |    debug.liveCode()
                  |    return "return"
                  |}
                  |
                  |debug(f2()).is("return")
                  |
                  |{
                  |    function f2() {
                  |        debug.liveCode()
                  |        return "inner"
                  |    }
                  |
                  |    debug(f2()).is("inner")
                  |}
                  |
                  |debug(f2()).is("inner")
                  |
                  |debug(f3()).is("return2")
                  |
                  |function f3() {
                  |    debug.liveCode()
                  |    return "return2"
                  |}
                  |
                  |debug(f3()).is("return2")
                  |
                  |{
                  |    debug(f3()).is("inner2")
                  |
                  |    function f3() {
                  |        debug.liveCode()
                  |        return "inner2"
                  |    }
                  |    debug(f3()).is("inner2")
                  |}
                  |
                  |
                  |debug(f3()).is("inner2")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "If the condition is concrete, if should disregard the false branch" in {
            val code =
                """/*
                  |    name: if control flow
                  |    desc: If the condition is concrete, if should disregard the false branch
                  | */
                  |
                  |var t = true
                  |var f = false
                  |var b = debug.boolean
                  |var result
                  |
                  |if(t) {
                  |    debug.liveCode()
                  |} else {
                  |    debug.deadCode()
                  |}
                  |
                  |if(f) {
                  |    debug.deadCode()
                  |} else {
                  |    debug.liveCode()
                  |}
                  |
                  |if(b) {
                  |    debug.liveCode()
                  |} else {
                  |    debug.liveCode()
                  |}
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Sequence expression should execute all expressions in its sequence" in {
            val code =
                """/*
                  |    name: sequence expr
                  |    desc: Sequence expression should execute all expressions in its sequence
                  | */
                  |
                  |var a = (1, 2)
                  |debug(a).is(2)
                  |
                  |var b = ("test", 4, "last")
                  |debug(b).is("last")
                  |
                  |if (true, false) {
                  |    debug.deadCode()
                  |} else {
                  |    debug.liveCode()
                  |}
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "If the condition is concrete, while should disregard the respective branch" in {
            val code =
                """/*
                  |    name: while control flow
                  |    desc: If the condition is concrete, while should disregard the respective branch
                  | */
                  |
                  |
                  |var t = true
                  |var f = false
                  |var b = debug.boolean
                  |
                  |while(f) {
                  |    debug.deadCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |while(b) {
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |while(t) {
                  |    debug.liveCode()
                  |}
                  |
                  |debug.deadCode()
                  |
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


}
