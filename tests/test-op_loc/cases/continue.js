var a = 1;

outer: while (a > 1) {
  while (a < 1) {
    continue outer;
  }
}

/* EXPECT(outer):
        loc 5:5
        goto 1:40
*/
