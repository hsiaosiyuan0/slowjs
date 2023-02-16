class c {
  f() {}
}

new c().f();

/* EXPECT(c.f()):
        col_num 9
        get_field2 f
        col_num 10
        call_method 0
*/
