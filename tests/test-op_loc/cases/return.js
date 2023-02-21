function f() {
  return print(1);
}

function f1() {
  return;
}

/* EXPECT(return print(1)):
        loc 2:15
        call 1
        return
*/

/* EXPECT(return):
        loc 6:3
        return_undef
*/
