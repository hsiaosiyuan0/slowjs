var b = [];
for (const [a, { b: c = 1 }] of b) {
}

/* EXPECT(a):
        col_num 13
        put_loc 1: a
*/

/* EXPECT(c):
  134:  col_num 21
        put_loc 2: c
*/

/* EXPECT(of b):
  168:  col_num 2:33
        for_of_next 0
*/
