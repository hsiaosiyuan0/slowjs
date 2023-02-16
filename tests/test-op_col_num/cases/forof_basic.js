var b = [];
for (const a of b) {
}

/* EXPECT(a):
   86:  col_num 12
        put_loc 1: a
*/

/* EXPECT(b):
  118:  col_num 2:17
        for_of_next 0
*/
