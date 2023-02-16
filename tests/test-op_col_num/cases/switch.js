var a = 1;

switch (a) {
  case "a":
    print("a");
    break;
  case "b":
    break;
  default:
    print(a);
}

/* EXPECT(case "a"):
        line_num 5
        col_num 5
        get_var print
        push_atom_value a
        col_num 5
        call 1
*/

/* EXPECT(break b):
        line_num 8
        col_num 5
        goto 1:230
*/

/* EXPECT(default):
        line_num 10
        col_num 5
        get_var print
        get_var a
        col_num 5
        call 1
*/
