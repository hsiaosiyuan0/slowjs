var a = 1;
let b = 2;
const c = 3,
  d = 4;
let e;
var f;  

/* EXPECT(a):
        col_num 9
        put_var a
*/

/* EXPECT(b):
        col_num 9
        put_var_init b
*/

/* EXPECT(c):
        col_num 11
        put_var_init c
*/

/* EXPECT(d):
        col_num 7
        put_var_init d
*/

/* EXPECT(e):
        col_num 5
        put_var_init e
*/
