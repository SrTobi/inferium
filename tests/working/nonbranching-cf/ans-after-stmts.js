/*
    name: ans from statement
    desc: After an statement, ans should have the correct value
 */

debug.ans.isOneOf(undefined);

"a"
{
    // nothing
}
debug.ans.isOneOf("a");


"b"
{
    "c"
}
debug.ans.isOneOf("c");


"d"
try {
    "e"
} catch (e) {
    "f"
}
debug.ans.isOneOf("e");


"g"
try {
    "h"
} finally {
    "i"
}
debug.ans.isOneOf("i");
