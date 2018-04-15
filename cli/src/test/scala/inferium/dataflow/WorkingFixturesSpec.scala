
package inferium.dataflow

import inferium.debug.{Fixture, FixtureRunner}
import org.scalatest.{FreeSpec, Matchers}

class WorkingFixturesSpec extends FreeSpec with Matchers {
            
    "/nonbranching-cf" - {
        "After an statement, ans should have the correct value" in {
            val code =
                """
                  |/*
                  |    name: ans from statement
                  |    desc: After an statement, ans should have the correct value
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
                  |var c = "c"
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
                """.stripMargin

            FixtureRunner.test(code)
        }
            
    }


}
