const [d, { e: f } = { e: 1 }] = [];
const { h: { i = 1 } = {} } = {};

/* EXPECT(d):
ident: 'd' 1:8
*/

/* EXPECT(e-alias):
ident: 'e' 1:13
*/

/* EXPECT(e-default):
ident: 'e' 1:24
*/

/* EXPECT(f):
ident: 'f' 1:16
*/

/* EXPECT(h):
ident: 'h' 2:9
*/

/* EXPECT(i):
ident: 'i' 2:14
*/

/* EXPECT([-rhs):
token: '[' 1:34
*/

/* EXPECT((-rhs):
token: '{' 2:31
*/
