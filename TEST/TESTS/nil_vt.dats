#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"

implement
main {n} (argc, argv) = 0 where {

val x = 
(
let

  fun loop
  ( 
    argc: int n, argv: !argv(n), i: natLte(n), res: List0_vt(string)
  )
  : List0_vt(string) = 

    if argc > i then
      loop(argc, argv, i+1, cons_vt($UN.cast{string}(argv[i]), res))

    else res
in
  list_vt_reverse(loop(argc, argv, 1, nil()))
end
)

val () = println!("x = ", x)
val () = free(x)

}