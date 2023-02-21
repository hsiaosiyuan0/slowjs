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
        loc 3:14
        insert2
        put_field a
*/

/* EXPECT(new a.f1(1)):
        loc 14:11
        get_field f1
        dup
        push_i32 1
        loc 14:13
        call_constructor 1
*/

/* EXPECT(new c().f2()):
        get_var c
        dup
        loc 15:10
        call_constructor 0
        loc 15:13
        get_field2 f2
        loc 15:15
        call_method 0
*/
