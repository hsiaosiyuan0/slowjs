var b = [];
for (const [a, { b: c = 1 }] of b) {
}

/* EXPECT(]):
token: ']' 1:10
*/

/* EXPECT(a):
ident: 'a' 2:13
*/

/* EXPECT(b):
ident: 'b' 2:18
*/

/* EXPECT(c):
ident: 'c' 2:21
*/

/* EXPECT(1):
number: 1 2:25
*/