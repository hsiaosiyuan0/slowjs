var b;
for (const { a, b: c } in b) {
}

/* EXPECT(a):
        loc 2:14
        put_loc 1: a
*/

/* EXPECT(b):
        loc 2:20
        put_loc 2: c
*/

/* EXPECT(in b):
        loc 2:27
        for_in_next
*/
