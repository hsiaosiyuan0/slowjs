const f = () => {};
class c {
  a = f();
}

/* EXPECT(a = f())):
        get_var f
        col_num 7
        call 0
*/
