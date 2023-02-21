const fn = () => {};
const obj = { f1: fn, b: { f2: fn } };
// ·f, = ·{}, ·fn()
const { d, e: f = { e: 2 } } = { d: fn() };
obj.f1();
obj.b.f2();
obj["f1"]();
obj["b"]["f2"]();

/* EXPECT(()=>{}):
        loc 1:12
        put_var_init fn
*/

/* EXPECT(e: f):
        loc 4:15
        put_var_init f
*/

/* EXPECT(d: fn):
        get_var fn
        loc 4:39
        call 0
*/

/* EXPECT(obj.f1()):
        loc 5:5
        get_field2 f1
        loc 5:7
        call_method 0
*/

/* EXPECT(obj.b.f2()):
        loc 6:5
        get_field b
        loc 6:7
        get_field2 f2
        loc 6:9
        call_method 0
*/

/* EXPECT(obj["f1"]()):
        get_var obj
        push_atom_value f1
        get_array_el2
        loc 7:10
        call_method 0
*/

/* EXPECT(obj["b"]["f2"]()):
        get_array_el
        push_atom_value f2
        get_array_el2
        loc 8:15
        call_method 0
*/
