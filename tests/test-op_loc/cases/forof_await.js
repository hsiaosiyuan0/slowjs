var b = [];
async function f() {
  for await (const a of b) {
  }
}

/* EXPECT(a):
        loc 3:20
        put_loc 0: a
*/

/* EXPECT(of b):
        loc 3:25
        iterator_get_value_done
*/
