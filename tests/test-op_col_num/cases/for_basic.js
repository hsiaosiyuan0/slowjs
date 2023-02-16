// prettier-ignore
for (let a = 1, b, c; 
  a < 10; a++, b++) {
  c += 1;
}

/* EXPECT(let a = 1):
        push_i32 1
        col_num 14
        put_loc 1: a
*/

/* EXPECT(a < 10):
        line_num 3
        get_loc_check 1: a
        push_i32 10
        lt
        col_num 3:3
        if_false
*/

/* EXPECT(a++):
        col_num 3:11
        get_loc_check 1: a
        post_inc
*/

/* EXPECT(c += 1):
        col_num 8
        dup
        put_loc_check 3: c
*/
