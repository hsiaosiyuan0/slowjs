var a = 1;

outer: while (a > 1) {
  while (a < 1) {
    break outer;
  }
}

/* EXPECT(outer):
ident: 'outer' 5:11
*/
