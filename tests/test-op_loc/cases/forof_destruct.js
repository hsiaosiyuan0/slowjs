var b = [];
for (const [a, { b: c = 1 }] of b) {
}

/* EXPECT(a):
        loc 2:13
        put_loc 1: a
*/

/* EXPECT(c):
        loc 2:21
        put_loc 2: c
*/

/* EXPECT(of b):
        loc 2:33
        for_of_next 0
*/
