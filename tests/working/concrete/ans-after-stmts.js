/*
    name: ans from statement
    desc: After a statement, ans should have the correct value
 */

debug.ans.is(undefined);

"a"
{
    // nothing
}
debug.ans.is("a");


"b"
{
    "c"
}
debug.ans.is("c");


"d"
try {
    "e"
} catch (e) {
    "f"
}
debug.ans.is("e");


"g"
try {
    "h"
} finally {
    "i"
}
debug.ans.is("i");
