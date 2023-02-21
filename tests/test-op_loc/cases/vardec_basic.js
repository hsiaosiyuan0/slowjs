var a = 1;
let b = 2;
const c = 3,
  d = 4;
let e;
var f;  

/* EXPECT(a):
        loc 1:9
        put_var a
*/

/* EXPECT(b):
        loc 2:9
        put_var_init b
*/

/* EXPECT(c):
        loc 3:11
        put_var_init c
*/

/* EXPECT(d):
        loc 4:7
        put_var_init d
*/

/* EXPECT(e):
        loc 5:5
        put_var_init e
*/
