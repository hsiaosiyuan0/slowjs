var b;
for (const { a, b: c } in b) {
}

/* EXPECT(a):
ident: 'a' 2:14
*/

/* EXPECT(b):
ident: 'b' 2:17
*/

/* EXPECT(c):
ident: 'c' 2:20
*/
