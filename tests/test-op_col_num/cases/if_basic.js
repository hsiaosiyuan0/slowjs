var a = 1;

if (print(a)) {
} else if ("😀" === a) {
  print(2);
}

/* EXPECT(print(a)):
        get_var print
        get_var a
        col_num 5
*/

/* EXPECT(print(2)):
        line_num 5
        col_num 3
        get_var print
        push_i32 2
        col_num 3
        call 1
*/
