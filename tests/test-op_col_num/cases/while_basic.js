var a = 0,
  b = 10;

while (a > b) {
  a++;
}

/* EXPECT(a++):
        line_num 5
        col_num 3
*/

/* EXPECT(a > b):
        gt
        col_num 4:8
        if_false 3:159
*/
