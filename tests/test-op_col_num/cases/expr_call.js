const fn = () => {};
const obj = { f1: fn, b: { f2: fn } };
// ·f, = ·{}, ·fn()
const { d, e: f = { e: 2 } } = { d: fn() };
obj.f1();
obj.b.f2();
obj["f1"]();
obj["b"]["f2"]();

/* EXPECT(()=>{}):
        col_num 12
        put_var_init fn
*/

/* EXPECT(f):
        col_num 15
        put_var_init f
*/

/* EXPECT(fn):
        get_var fn
        col_num 37
        call 0
*/

/* EXPECT(obj.f1()):
        get_var obj
        col_num 5
        get_field2 f1
        col_num 7
        call_method 0
*/

/* EXPECT(obj.b.f2()):
        get_var obj
        col_num 5
        get_field b
        col_num 7
        get_field2 f2
        col_num 9
        call_method 0
*/

/* EXPECT(obj["f1"]()):
        get_var obj
        push_atom_value f1
        get_array_el2
        col_num 10
*/

/* EXPECT(obj["b"]["f2"]()):
        push_atom_value f2
        get_array_el2
        col_num 15
        call_method 0
*/
