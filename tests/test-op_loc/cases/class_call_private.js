class c {
  #b = () => {};

  f() {
    this.#b();
  }
}

/* EXPECT(this.#b()):
        loc 5:10
        dup
        get_var_ref 0: "#b"
        get_private_field


        loc 5:12
        call_method 0
*/
