var a = 1;

outer: while (a > 1) {
  while (a < 1) {
    continue outer;
  }
}

/* EXPECT(outer):
        line_num 5
        col_num 5
        goto 1:55
*/
