var a = 1;

switch (a) {
  case "a":
    break;
  default:
    print(a);
}

/* EXPECT(a):
ident: 'a' 3:9
*/

/* EXPECT(:):
token: ':' 6:10
*/

/* EXPECT(a in print):
ident: 'a' 7:11
*/
