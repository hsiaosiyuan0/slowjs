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
        loc 5:5
        get_var print
        push_atom_value a
        loc 5:10
        call 1
*/

/* EXPECT(break b):
        loc 8:5
        goto 1:175
*/

/* EXPECT(default):
        loc 10:5
        get_var print
        get_var a
        loc 10:10
        call 1
*/
