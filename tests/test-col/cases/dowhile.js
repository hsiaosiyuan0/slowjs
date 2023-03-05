var a = 1;

do {
  print(a);
} while (a < 0);

/* EXPECT(a):
token: '{' 3:4
*/

/* EXPECT(a):
ident: 'a' 4:9
*/

/* EXPECT(a <):
ident: 'a' 5:10
*/

/* EXPECT(0):
number: 0 5:14
*/