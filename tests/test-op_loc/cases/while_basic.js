var a = 0,
  b = 10;

while (a > b) {
  a++;
}

/* EXPECT(a++):
        loc 5:3
        get_var a
        post_inc
*/

/* EXPECT(a > b):
        get_var b
        gt
        loc 4:8
        if_false 3:129
*/
