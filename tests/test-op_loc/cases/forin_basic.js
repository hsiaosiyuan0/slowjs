var b;
for (const a in b) {
  a += 2;
}

/* EXPECT(const a):
        loc 2:12
        put_loc 1: a
*/

/* EXPECT(in b):
        loc 2:17
        for_in_next
*/

/* EXPECT(a += 2):
        get_ref_value
        push_i32 2
        add
        loc 3:8
        label 5:103
  103:  insert3
        put_ref_value
*/
