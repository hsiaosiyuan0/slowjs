var b;
for (const a in b) {
  a += 2;
}

/* EXPECT(const a):
        col_num 12
        put_loc 1: a
*/

/* EXPECT(in b):
  133:  col_num 2:17
        for_in_next
*/

/* EXPECT(a += 2):
   77:  line_num 3
        col_num 3
        throw_error a,0
        get_ref_value
        push_i32 2
        add
        col_num 8
        label 5:118
  118:  insert3
        put_ref_value
*/
