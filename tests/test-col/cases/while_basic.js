var a = 0,
  b = 10;

while (a > b) {}

/* EXPECT(a):
ident: 'a' 4:8
*/

/* EXPECT(b):
ident: 'b' 4:12
*/