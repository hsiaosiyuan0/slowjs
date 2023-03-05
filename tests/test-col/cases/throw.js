try {
  throw "test😀";
} catch (error) {}

/* EXPECT({ after try):
token: '{' 1:5
*/

/* EXPECT(; term throw):
token: ';' 2:16
*/

/* EXPECT(error):
ident: 'error' 3:10
*/
