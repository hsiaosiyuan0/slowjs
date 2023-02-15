var a = 1;

if (print(a)) {
} else if ("😀" === a) {
}

/* EXPECT(a after emoji):
ident: 'a' 4:20
*/

/* EXPECT(a):
ident: 'a' 3:11
*/