// prettier-ignore
for (let a = 1, b, c; 
  a < 10; a++, b++) {
  c += 1;
}

/* EXPECT(let a = 1):
        push_i32 1
        loc 2:14
        put_loc 1: a
*/

/* EXPECT(a < 10):
        get_loc_check 1: a
        push_i32 10
        lt
        loc 3:3
        if_false 3:168
*/

/* EXPECT(a++):
        loc 3:11
        get_loc_check 1: a
        post_inc
*/

/* EXPECT(c += 1):
        loc 4:3
        get_loc_check 3: c
        push_i32 1
        add
        loc 4:8
        dup
        put_loc_check 3: c
*/
