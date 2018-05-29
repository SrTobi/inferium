
package inferium.dataflow

import inferium.debug.{Fixture, FixtureRunner}
import org.scalatest.{FreeSpec, Matchers}

class WorkingFixturesSpec extends FreeSpec with Matchers {
            
    "/abstract" - {
        "Checks if conditional branching filters chained properties" in {
            val code =
                """/*
                  |    name: chain filtering
                  |    desc: Checks if conditional branching filters chained properties
                  | */
                  |
                  |var a = debug.boolean
                  |var b = a
                  |var c = b
                  |var d = a
                  |
                  |if(a) {
                  |    debug(a).isOneOf(true)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |    debug(d).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(false)
                  |    debug(c).isOneOf(false)
                  |    debug(d).isOneOf(false)
                  |}
                  |
                  |
                  |if(c) {
                  |    debug(a).isOneOf(true)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |    debug(d).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(false)
                  |    debug(c).isOneOf(false)
                  |    debug(d).isOneOf(false)
                  |}
                  |
                  |if(d) {
                  |    debug(a).isOneOf(true)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |    debug(d).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(false)
                  |    debug(c).isOneOf(false)
                  |    debug(d).isOneOf(false)
                  |}
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Checks if conditional branching filters properties respecting union values" in {
            val code =
                """/*
                  |    name: fan filtering
                  |    desc: Checks if conditional branching filters properties respecting union values
                  | */
                  |
                  |var a = false
                  |var b = true
                  |var c = true
                  |
                  |var d = debug.squash(a, b)
                  |var e = debug.squash(c, d)
                  |
                  |debug(a).isOneOf(false)
                  |debug(b).isOneOf(true)
                  |debug(c).isOneOf(true)
                  |
                  |debug(d).isOneOf(debug.boolean)
                  |debug(e).isOneOf(debug.boolean)
                  |
                  |if (d) {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(true)
                  |    debug(e).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(false)
                  |    debug(e).isOneOf(debug.boolean)
                  |}
                  |
                  |if (e) {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(debug.boolean)
                  |    debug(e).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(false)
                  |    debug(e).isOneOf(false)
                  |}
                  |
                  |a = true
                  |
                  |if (e) {
                  |    debug(a).isOneOf(true)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(debug.boolean)
                  |    debug(e).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(true)
                  |    debug(b).isOneOf(true)
                  |    debug(c).isOneOf(true)
                  |
                  |    debug(d).isOneOf(false)
                  |    debug(e).isOneOf(false)
                  |}
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Checks if conditional branching filters the condition" in {
            val code =
                """/*
                  |    name: filtering
                  |    desc: Checks if conditional branching filters the condition
                  | */
                  |
                  |var a = debug.boolean
                  |
                  |if (a) {
                  |    debug(a).isOneOf(true)
                  |} else {
                  |    debug(a).isOneOf(false)
                  |}
                  |
                  |debug(a).isOneOf(debug.boolean)
                  |
                  |
                  |var b = debug.number
                  |
                  |if (b) {
                  |    debug(b).isOneOf(debug.number)
                  |} else {
                  |    debug(b).isOneOf(0)
                  |}
                  |
                  |debug(b).isOneOf(debug.number)
                  |
                  |
                  |var c = debug.string
                  |
                  |if (c) {
                  |    debug(c).isOneOf(debug.string)
                  |} else {
                  |    debug(c).isOneOf("")
                  |}
                  |
                  |debug(c).isOneOf(debug.string)
                  |
                  |
                  |var d = undefined
                  |
                  |if (debug.boolean) {
                  |    d = debug.number
                  |}
                  |
                  |if (debug.boolean) {
                  |    d = "test"
                  |} else {
                  |    d = ""
                  |}
                  |
                  |debug(d).isOneOf(undefined, debug.number, "test", "")
                  |if (d) {
                  |    debug(d).isOneOf(debug.number, "test")
                  |} else {
                  |    debug(d).isOneOf(undefined, 0, "")
                  |}
                  |
                  |debug(d).isOneOf(undefined, debug.number, "test", "")
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "Checks if conditional branching filters the base object of references" in {
            val code =
                """/*
                  |    name: Object filtering
                  |    desc: Checks if conditional branching filters the base object of references
                  | */
                  |
                  |var t = { cond: true }
                  |var f = { cond: false }
                  |var b = { cond: debug.boolean }
                  |
                  |if (debug.boolean) {
                  |    var test = t
                  |} else {
                  |    test = f
                  |}
                  |
                  |debug(test).isOneOf(t, f)
                  |debug(test.cond).isOneOf(debug.boolean)
                  |
                  |if (test.cond) {
                  |    debug(test).isOneOf(t)
                  |    debug(t.cond).isOneOf(true)
                  |    debug(f.cond).isOneOf(false)
                  |} else {
                  |    debug(test).isOneOf(f)
                  |    debug(t.cond).isOneOf(true)
                  |    debug(f.cond).isOneOf(false)
                  |}
                  |
                  |debug(test).isOneOf(t, f)
                  |
                  |if (debug.boolean) {
                  |    test = b
                  |}
                  |
                  |debug(test).isOneOf(t, f, b)
                  |
                  |if (test.cond) {
                  |    debug(test).isOneOf(t, b)
                  |    debug(t.cond).isOneOf(true)
                  |    debug(f.cond).isOneOf(false)
                  |} else {
                  |    debug(test).isOneOf(f, b)
                  |    debug(t.cond).isOneOf(true)
                  |    debug(f.cond).isOneOf(false)
                  |}
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
                  |    debug(lastObj.prop).isOneOf("init")
                  |    lastObj.prop = "next"
                  |    debug(lastObj.prop).isOneOf("next")
                  |
                  |
                  |    if (debug.boolean) {
                  |        someObj.prop = "blub"
                  |    }
                  |}
                  |
                  |debug(lastObj.prop).isOneOf("next", "blub")
                  |debug(someObj.prop).isOneOf("next", "blub")
                  |
                  |someObj.absProp = "abs"
                  |debug(someObj.absProp).isOneOf("abs", undefined)
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
                  |debug(o.prop).isOneOf("yes", "no")
                  |
                  |o.prop = "clear"
                  |
                  |if (debug.boolean) {
                  |    o.prop = "then"
                  |}
                  |
                  |debug(o.prop).isOneOf("clear", "then")
                  |
                  |
                  |o.prop = "clear"
                  |if (debug.boolean) {
                  |
                  |} else {
                  |    o.prop = "else"
                  |}
                  |
                  |debug(o.prop).isOneOf("clear", "else")
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
                  |debug(a.prop).isOneOf("a")
                  |debug(b.prop).isOneOf("b")
                  |debug(o.prop).isOneOf("a", "b")
                  |
                  |a.prop = "A"
                  |debug(a.prop).isOneOf("A")
                  |debug(b.prop).isOneOf("b")
                  |debug(o.prop).isOneOf("A", "b")
                  |
                  |o.prop = "O"
                  |debug(a.prop).isOneOf("A", "O")
                  |debug(b.prop).isOneOf("b", "O")
                  |debug(o.prop).isOneOf("A", "b", "O")
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
                  |debug.ans.isOneOf(undefined);
                  |
                  |"a"
                  |{
                  |    // nothing
                  |}
                  |debug.ans.isOneOf("a");
                  |
                  |
                  |"b"
                  |{
                  |    "c"
                  |}
                  |debug.ans.isOneOf("c");
                  |
                  |
                  |"d"
                  |try {
                  |    "e"
                  |} catch (e) {
                  |    "f"
                  |}
                  |debug.ans.isOneOf("e");
                  |
                  |
                  |"g"
                  |try {
                  |    "h"
                  |} finally {
                  |    "i"
                  |}
                  |debug.ans.isOneOf("i");
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
                  |debug.ans.isOneOf(undefined);
                  |
                  |undefined;
                  |debug.ans.isOneOf(undefined);
                  |
                  |null;
                  |debug.ans.isOneOf(null);
                  |
                  |0;
                  |debug.ans.isOneOf(0);
                  |debug.ans.isOneOf(debug.number);
                  |
                  |//(-8);
                  |//debug.ans.isOneOf(-8);
                  |//debug.ans.isOneOf(debug.number);
                  |
                  |"test";
                  |debug.ans.isOneOf("test");
                  |debug.ans.isOneOf(debug.string);
                  |
                  |"";
                  |debug.ans.isOneOf("");
                  |debug.ans.isOneOf(debug.string);
                  |
                  |true;
                  |debug.ans.isOneOf(true);
                  |debug.ans.isOneOf(debug.boolean);
                  |
                  |false;
                  |debug.ans.isOneOf(false);
                  |debug.ans.isOneOf(debug.boolean);
                  |
                  |
                  |// test the debug helpers
                  |debug.boolean;
                  |debug.ans.isOneOf(debug.boolean);
                  |
                  |debug.number;
                  |debug.ans.isOneOf(debug.number);
                  |
                  |debug.string;
                  |debug.ans.isOneOf(debug.string);
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
                  |debug.ans.isOneOf("use strict")
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
                  |debug.ans.isOneOf("a")
                  |
                  |var b = "b"
                  |
                  |debug(a).isOneOf("a")
                  |debug(b).isOneOf("b")
                  |
                  |debug(c).isOneOf(undefined)
                  |c = "c"
                  |debug(c).isOneOf("c")
                  |var c
                  |debug(c).isOneOf("c")
                  |
                  |debug(d).isOneOf(undefined)
                  |{
                  |    var d = "d"
                  |    debug(d).isOneOf("d")
                  |}
                  |debug(d).isOneOf("d")
                  |
                  |
                  |{
                  |    let d = "not d"
                  |    debug(d).isOneOf("not d")
                  |
                  |    {
                  |        debug(d).isOneOf("not d")
                  |        let d = "d indeed"
                  |        debug(d).isOneOf("d indeed")
                  |    }
                  |    debug(d).isOneOf("not d")
                  |}
                  |debug(d).isOneOf("d")
                  |
                  |var e = "e"
                  |{
                  |    debug(e).isOneOf("e")
                  |    let e = "e-inner"
                  |    debug(e).isOneOf("e-inner")
                  |}
                  |debug(e).isOneOf("e")
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
                  |debug(o).isOneOf(o)
                  |
                  |o.a = "a"
                  |o.b = "b"
                  |
                  |debug(o.a).isOneOf("a")
                  |debug(o.b).isOneOf("b")
                  |
                  |
                  |var o2 = { a: "a", b: "b" }
                  |debug(o2.a).isOneOf("a")
                  |debug(o2.b).isOneOf("b")
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
                  |debug(o.prop).isOneOf("prop")
                  |
                  |o.prop = "something else"
                  |debug(o.prop).isOneOf("something else")
                  |
                  |{
                  |    const a = o
                  |    a.prop = "written by a"
                  |}
                  |debug(o.prop).isOneOf("written by a")
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
                  |debug(ths).isOneOf(g)
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
                  |debug(res1).isOneOf("then")
                  |
                  |var res2 = f? debug("never").deadCode() : "else"
                  |debug(res2).isOneOf("else")
                  |
                  |var res3 = b? "then" : "else"
                  |debug(res3).isOneOf("then", "else")
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
                  |        debug(a).isOneOf("init")
                  |        a = "changed"
                  |        continue snd
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug(a).isOneOf("init")
                  |    debug.liveCode()
                  |}
                  |
                  |debug.liveCode()
                  |
                  |third: while (debug.boolean) {
                  |    debug.liveCode()
                  |
                  |    var b = "init"
                  |
                  |    inner: while (debug.boolean) {
                  |        debug.liveCode()
                  |        debug(b).isOneOf("init")
                  |        b = "changed"
                  |        continue third
                  |        debug.deadCode()
                  |    }
                  |
                  |    debug(b).isOneOf("init", "changed")
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
                  |    debug(a).isOneOf("in try").mightBeDead()
                  |    a = "in finally"
                  |}
                  |
                  |debug(a).isOneOf("in finally")
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
                  |        debug(b).isOneOf("in inner try").mightBeDead()
                  |        b = "in inner finally"
                  |    }
                  |    debug.deadCode()
                  |} finally {
                  |    debug(b).isOneOf("in inner finally").mightBeDead()
                  |    b = "in finally"
                  |}
                  |
                  |debug(b).isOneOf("in finally")
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
                  |    debug(a).isOneOf("init", "in finally")
                  |    try {
                  |        a = "in try"
                  |        continue
                  |        debug.deadCode()
                  |    } finally {
                  |        debug(a).isOneOf("in try").mightBeDead()
                  |        a = "in finally"
                  |    }
                  |    debug.deadCode()
                  |}
                  |
                  |debug(a).isOneOf("init", "in finally")
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
                  |debug(a).isOneOf("in finally")
                  |
                  |
                  |let b = "init"
                  |
                  |try {
                  |    b = "in try"
                  |    try {
                  |        debug(b).isOneOf("in try")
                  |        b = "in inner try"
                  |    } finally {
                  |        debug(b).isOneOf("in inner try")
                  |        b = "in inner finally"
                  |    }
                  |} finally {
                  |    debug(b).isOneOf("in inner finally")
                  |    b = "in finally"
                  |}
                  |
                  |debug(b).isOneOf("in finally")
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
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


}
