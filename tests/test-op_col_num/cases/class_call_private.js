class c {
  #b = () => {};

  f() {
    this.#b();
  }
}

/* EXPECT(this.#b()):
        get_loc 0: this
        col_num 10
        dup
        get_var_ref 0: "#b"
        get_private_field
        col_num 12
        call_method 0
*/
