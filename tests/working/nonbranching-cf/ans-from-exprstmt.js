/*
    name: ans from Expression statement
    desc: After an expression statement, ans should be the result of the expression
 */

// when the program is started ans should be undefined
debug.ans.isOneOf(undefined);

undefined;
debug.ans.isOneOf(undefined);

null;
debug.ans.isOneOf(null);

0;
debug.ans.isOneOf(0);
debug.ans.isOneOf(debug.number);

//(-8);
//debug.ans.isOneOf(-8);
//debug.ans.isOneOf(debug.number);

"test";
debug.ans.isOneOf("test");
debug.ans.isOneOf(debug.string);

"";
debug.ans.isOneOf("");
debug.ans.isOneOf(debug.string);

true;
debug.ans.isOneOf(true);
debug.ans.isOneOf(debug.boolean);

false;
debug.ans.isOneOf(false);
debug.ans.isOneOf(debug.boolean);