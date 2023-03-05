for (let a = 1, b, c; a < 10; a++, b++) {
  c += 1;
}

/* EXPECT(let a = 1):
ident: 'let' 1:6
ident: 'a' 1:10
token: '=' 1:12
number: 1 1:14
*/

/* EXPECT(b):
ident: 'b' 1:17
*/

/* EXPECT(c in c += 1):
ident: 'c' 2:3
*/

/* EXPECT(1 in c += 1):
number: 1 2:8
*/
