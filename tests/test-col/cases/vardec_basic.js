var a = 1;
let b = 1,
  c;
const d = 1;

const fn = () => {};
const obj = { d: fn() };

/* EXPECT(a):
ident: 'a' 1:5
token: '=' 1:7
number: 1 1:9
*/

/* EXPECT(b):
ident: 'b' 2:5
token: '=' 2:7
number: 1 2:9
*/

/* EXPECT(c):
ident: 'c' 3:3
*/

/* EXPECT(d):
ident: 'd' 4:7
token: '=' 4:9
number: 1 4:11
*/

/* EXPECT(fn):
ident: 'fn' 7:18
*/
