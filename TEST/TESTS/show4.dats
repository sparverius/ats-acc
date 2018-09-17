#include "share/atspre_staload.hats"

(* typedef X = @{ a= int, b=string } *)
(* typedef X = $tup(int, string) *)
typedef X = '{ a=int, b=string }

val x = cons_vt('c', nil_vt()) : List0_vt(X)
val _ = $showtype x


implement main0() = ()