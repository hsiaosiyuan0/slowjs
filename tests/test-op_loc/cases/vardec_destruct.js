// ·d, ·f
const [d, { e: f } = { e: 2 }] = [];

// ·i, = ·{}
const { h: { i = 1 } = {} } = {};

/* EXPECT(d):
        loc 2:8
        put_var_init d
*/

/* EXPECT(f):
        loc 2:16
        put_var_init f
*/

/* EXPECT(i):
        loc 5:14
        put_var_init i
*/
