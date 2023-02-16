function f() {
  return print(1);
}

function f1() {
  return;
}

/* EXPECT(return print(1)):
        line_num 2
        get_var print
        push_i32 1
        col_num 10
        call 1
        return
*/

/* EXPECT(return):
        line_num 6
        col_num 3
        return_undef
*/
