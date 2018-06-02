/*
    name: ans from Expression statement
    desc: After an expression statement, ans should be the result of the expression
 */

// when the program is started ans should be undefined
debug.ans.is(undefined);

undefined;
debug.ans.is(undefined);

null;
debug.ans.is(null);

0;
debug.ans.is(0);

//(-8);
//debug.ans.is(-8);
//debug.ans.is(debug.number);

"test";
debug.ans.is("test");

"";
debug.ans.is("");

true;
debug.ans.is(true);

false;
debug.ans.is(false);


// test the debug helpers
debug.boolean;
debug.ans.is(debug.boolean);

debug.number;
debug.ans.is(debug.number);

debug.string;
debug.ans.is(debug.string);
