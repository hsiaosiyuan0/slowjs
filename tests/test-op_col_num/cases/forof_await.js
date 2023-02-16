var b = [];
async function f() {
  for await (const a of b) {
  }
}

/* EXPECT(a):
   34:  col_num 20
        put_loc 0: a
*/

/* EXPECT(of b):
        await
        col_num 3:25
        iterator_get_value_done
*/
