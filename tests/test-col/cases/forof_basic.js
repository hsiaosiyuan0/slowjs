var b = [];
for (const a of b) {
}

/* EXPECT(]):
token: ']' 1:10
*/

/* EXPECT(a):
ident: 'a' 2:12
*/

/* EXPECT(b):
ident: 'b' 2:17
*/
