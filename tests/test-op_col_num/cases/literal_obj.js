const fn = () => {};
// ·f, = ·{}, ·fn()
const { d, e: f = { e: 2 } } = { d: fn() };

/* EXPECT(()=>{}):
        col_num 12
        put_var_init fn
*/

/* EXPECT(f):
        col_num 15
        put_var_init f
*/
