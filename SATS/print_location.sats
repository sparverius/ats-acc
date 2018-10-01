
#ifndef TOKENS_NONE

  #define TOKENS_NONE

  staload UN = "prelude/SATS/unsafe.sats"

  (* #include "./token.dats" *)
  (* #include "./tokenize.dats" *)
  (* #include "./mylib/bashstr.dats" *)
  (* #include "./token_lib.dats" *)
  (* #include "./tok_vt.dats" *)
  (* #include "./errkind.dats" *)
  (* #include "./classify_toks.dats" *)
  (* #include "./print_util.dats" *)
  (* #include "./simplify_print.dats" *)
  (* #include "./print_errkind.dats" *)

  staload "./token.sats"
  staload "./vtypes.sats"

#endif



typedef loc0 = (int, int, int, int)


fn
print_location
(xs: !List0_vt(char)): void 
(*   = let  *)
(*     implement(env) *)
(*     list_vt_foreach$fwork<char><env>(c,env)  *)
(*       = fprint(stdout_ref, c) *)
(* in *)
(*   list_vt_foreach(xs) *)
(* end *)


fn
print_location_colored
(xs: !List0_vt(char), loc: loc0): void

fn
go_to_point
(xs: !List0_vt(char), loc: loc0, color: bool): void


fn
get_point_path
(path: string, loc: loc0, color: bool): void

fn 
tokstr_to_chrlst
(s: !Strnptr): List0_vt(char) 


fn
string_make_list_vt0
(cs: List0(char)): Strnptr1


// for printing exact error location 
// i.e.
// val () = ( a :=: b )
//            ^~~~~~~
fn
print_arrow
(n: int, z: int, offset: int, color: bool): void

fn 
get_path
(path: !List_vt(token), loc: loc0, color: bool): void

(* ****** ****** *)

fn
print_path(xs: !toks): void

(*
fn
print_first_two1
(x0: !toks, x1: !toks, t: bool): void
*)
