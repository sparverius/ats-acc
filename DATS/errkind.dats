#ifndef TOKENS_NONE

  #include "./tokenize.dats"
  #include "./mylib/bashstr.dats"
  #include "./token_lib.dats"
  #include "./tok_vt.dats"

#endif

(* ****** ****** *)

staload "./../SATS/errkind.sats"
staload "./../SATS/errkind_util.sats"

(* ****** ****** *)


implement
print_errkind
(err) = fprint_errkind(stderr_ref, err)


implement
fprint_errkind
(out, err) = (fprint!(out, get_errkind_string(err)))


implement 
free_errkind
(x): void = {
  val _ = (
    case+ x of
      | ~ERRwarn   l => free_toks(l)
      | ~ERRparse  l => free_toks(l)
      | ~ERRother  l => free_toks(l)
      | ~ERRtyleq  l => free_toks(l)
      | ~ERRdynexp l => free_toks(l)
      | ~ERRdynvar l => free_toks(l)
      | ~ERRcstpat l => free_toks(l)
      | ~ERRunsolv l => free_toks(l)
      | ~ERRlexing l => free_toks(l)
      | ~ERRexit2  l => free_toks(l)
      | ~ERRsymbol l => free_toks(l)
      | ~ERRfound  l => free_toks(l)
      | ~ERRshow   l => free_toks(l)
      | ~ERRsorts  l => free_toks(l)
      | ~ERRsortu  l => free_toks(l)
      | ~ERRnonex  l => free_toks(l)
      | ~ERRlast   l => (free_toktup(l))
      | ~ERRunit   _ => ()
      //
      | ~ERRsimpre  l => free_toks(l)
  )
}


implement
get_errkind_string
(xs) = (
  case+ xs of
    | ERRwarn   _ => "ERRwarn"
    | ERRparse  _ => "ERRparse"
    | ERRother  _ => "ERRother"
    | ERRtyleq  _ => "ERRtyleq"
    | ERRdynexp _ => "ERRdynexp"
    | ERRdynvar _ => "ERRdynvar"
    | ERRcstpat _ => "ERRcstpat"
    | ERRunsolv _ => "ERRunsolv"
    | ERRlexing _ => "ERRlexing"
    | ERRexit2  _ => "ERRexit2"
    | ERRsymbol _ => "ERRsymbol"
    | ERRfound  _ => "ERRfound"
    | ERRlast   _ => "ERRlast"
    | ERRshow   _ => "ERRshow"
    | ERRsorts  _ => "ERRsorts"
    | ERRsortu  _ => "ERRsortu"
    | ERRnonex  _ => "ERRnonex"
    | ERRunit   _ => "ERRunit"
    //
    | ERRsimpre _ => "ERRstaimpre"

)


implement{} print_parse  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_other  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_tyleq  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_exit2  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_found  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_sorts  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_sortu  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_nonex  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_other  (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_dynexp (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_dynvar (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_cstpat (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_unsolv (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_lexing (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_symbol (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_show   (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_warn   (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))
implement{} print_unit   ((*    *)): void = ()
implement{} print_last (xs: toktup, color: bool, verbose: bool): void = (free_toktup(xs))
//
implement{} print_simpre (xs: toks, color: bool, verbose: bool): void = (free_toks(xs))


implement{}
print_errkind_single
(x2, color, verbose) = (
  case+ x2 of
  | ~ERRparse  x => print_parse<>(x, color, verbose)
  | ~ERRother  x => print_other<>(x, color, verbose)
  | ~ERRtyleq  x => print_tyleq<>(x, color, verbose)
  | ~ERRexit2  x => print_exit2<>(x, color, verbose)
  | ~ERRfound  x => print_found<>(x, color, verbose)
  | ~ERRsorts  x => print_sorts<>(x, color, verbose)
  | ~ERRsortu  x => print_sortu<>(x, color, verbose)
  | ~ERRnonex  x => print_nonex<>(x, color, verbose)
  | ~ERRdynexp x => print_dynexp<>(x, color, verbose)
  | ~ERRdynvar x => print_dynvar<>(x, color, verbose)
  | ~ERRcstpat x => print_cstpat<>(x, color, verbose)
  | ~ERRunsolv x => print_unsolv<>(x, color, verbose)
  | ~ERRlexing x => print_lexing<>(x, color, verbose)
  | ~ERRsymbol x => print_symbol<>(x, color, verbose)
  | ~ERRshow   x => print_show<>(x, color, verbose)
  | ~ERRlast   x => print_last<>(x, color, verbose)
  | ~ERRwarn   x => print_warn<>(x, color, verbose)
  | ~ERRunit   _ => print_unit<>( )
  //
  | ~ERRsimpre x => print_simpre<>(x, color, verbose)
) 


implement
free_errtup
(xs) = let
  val x0 = xs.0
  val x1 = xs.1
  val x2 = xs.2
in
  (free_toks(x0); free_toks(x1); free_errkind(x2))
end


implement
free_errtups
(xs) = let 
    fun auxmain(xs: errtups): void = 
          case+ xs of
          | ~nil_vt() => ()
          | ~cons_vt(x, xs) => (free_errtup(x); auxmain(xs))
    in
      auxmain(xs)
    end


(* ****** ****** *)

(* end of [errkind.dats] *)














