
package inferium.dataflow

import inferium.debug.{Fixture, FixtureRunner}
import org.scalatest.{FreeSpec, Matchers}

class WorkingFixturesSpec extends FreeSpec with Matchers {
            
    "/abstract" - {
        "Checks if conditional branching filters the condition" in {
            val code =
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
                  |    name: empty
                  |    desc: Empty script should be parsable
                  | */
                  |
                  |// nothing to do
                """.stripMargin

            FixtureRunner.test(code)
        }
            
        "If the condition is concrete, if should disregard the false branch" in {
            val code =
                """
                  |/*
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
            
        "Lexical read and write should respect scopes" in {
            val code =
                """
                  |/*
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
                """
                  |/*
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
                """
                  |/*
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
            
        "If the condition is concrete, while should disregard the respective branch" in {
            val code =
                """
                  |/*
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
