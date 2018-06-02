package inferium.testtools
object Resources {
    val files: Map[String, String] = Map(
        "js-test-env.js" -> raw"""var idgb;
{
    idgb = function(expr) {
        return {
            is: function() {
                var args = [].slice.call(arguments);
                args.some(function (arg) {
                    return arg === expr
                });
                throw new Error(expr + " wasn't equal to anything in " + args)
            }
        }
    };

    idgb.ans = {
        is: function () {
            // nothing to do
        }
    };

    idgb.deadCode = function() {
        throw new Error("Dead code should not have been executed")
    };

    idgb.liveCode = function() {

    }
}



{
    // <$$test-case$$>
}

// noinspection BadExpressionStatementJS
"ok";""",
    )
}
