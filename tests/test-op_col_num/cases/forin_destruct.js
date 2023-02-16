var b;
for (const { a, b: c } in b) {
}

/* EXPECT(a):
        col_num 14
        put_loc 1: a
*/

/* EXPECT(b):
        col_num 20
        put_loc 2: c
*/

/* EXPECT(in b):
  115:  col_num 2:27
        for_in_next
*/
