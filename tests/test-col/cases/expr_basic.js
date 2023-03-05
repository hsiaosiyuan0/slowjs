var a;

function b() {}

var c = { ["👌"]: () => {} };

a = new b();
a = a + b() + c["👌"]();

/* EXPECT(b after new):
ident: 'b' 7:9
*/

/* EXPECT(( after new b):
token: '(' 7:10
*/

/* EXPECT(( after b):
token: '(' 8:10
*/

/* EXPECT([ after c):
token: '[' 8:16
*/

/* EXPECT() after ]):
token: '(' 8:21
*/
