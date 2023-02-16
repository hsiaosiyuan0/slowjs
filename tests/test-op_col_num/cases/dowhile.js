var a = 1;

do {
  print(a);
} while (a < 0);

/* EXPECT(a < 0):
  104:  get_var a
        push_i32 0
        lt
        col_num 5:10
        if_true 3:51
*/
