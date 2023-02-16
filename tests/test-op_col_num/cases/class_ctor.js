class c {
  constructor(a) {
    this.a = a;
  }

  f2() {}
}

function f1() {}
const a = {
  f1,
};

1 + new a.f1(1);
2 + new c().f2();

/* EXPECT(= a):
        col_num 14
        insert2
        put_field a
*/

/* EXPECT(new a.f1(1)):
        get_field f1
        dup
        push_i32 1
        col_num 5
        call_constructor 1
*/

/* EXPECT(new c().f2()):
        col_num 5
        call_constructor 0
        col_num 13
        get_field2 f2
        col_num 15
        call_method 0
*/
