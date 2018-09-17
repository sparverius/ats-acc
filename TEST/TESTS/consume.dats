#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"

// strange that this does not throw error 
val x = "foo"
val x0 = string_append(" ", x)
val x1 = $UN.strptr2string(x0)
(* val () = strptr_free(x0) *)


// but this does
fn
foo_str(x: string): string = let
  val x0 = string_append(" ", x)
  val x1 = $UN.strptr2string(x0)
in
  x1
end

implement main0() = ()