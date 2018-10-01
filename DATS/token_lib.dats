#ifndef TOKENS_NONE

#include "./token.dats"
staload UN = "prelude/SATS/unsafe.sats"

#endif

staload "./../SATS/token_lib.sats"


implement
tok_is_ide(x) =
  case+ x of | TOKide _ => true | _ => false


implement
tok_is_col(x) =
  case+ x of | TOKcol _ => true | _ => false


implement
tok_is_s2e(x) =
  case+ x of | TOKs2e _ => true | _ => false

//


implement
strnptr_eq_string(i, s) = let
  val x0 = strnptr_copy(i)
  val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
  val x1 = $UN.strnptr2string(x0)
  val res = x1 = s
  val () = strnptr_free(x0)
in
  res
end


implement
tok_ide_eq(x, s) =
  case+ x of | TOKide i => strnptr_eq_string(i, s) | _ => false


implement
tok_int_eq(x, s) =
  case+ x of | TOKint i => strnptr_eq_string(i, s) | _ => false


implement
tok_s2e_eq(x, s) =
  case+ x of | TOKs2e i => strnptr_eq_string(i, s) | _ => false


implement
tok_chr_eq(x, c) =
  case+ x of | TOKchr i => i = c | _ => false


(* ****** ****** *)


implement
tok_chr_eq_int(x, c) =
  case+ x of | TOKchr chr => char2int0(chr) = c | _ => false


(* ****** ****** *)


implement
strnptr_eq_strnptr(i, j) = let
  val x0 = strnptr_copy(i)
  val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
  val x1 = $UN.strnptr2string(x0)

  val y0 = strnptr_copy(j)
  val _ = assertloc(g1int2int_ssize_int(length(y0)) > 0)
  val y1 = $UN.strnptr2string(y0)

  val res = (y1 = x1)
  val () = strnptr_free(x0)
  val () = strnptr_free(y0)
in
  res
end


(* ****** ****** *)


implement
tok_int_eq_tok_int(x, y) =
  case+ (x, y) of 
  | (TOKint i, TOKint j) => strnptr_eq_strnptr(i, j)
  | (_, _) => false


(* ****** ****** *)


implement
is_chr(tok) = 
  case+ tok of | TOKchr _ => true | _ => false

implement
is_col(tok) = 
  case+ tok of | TOKcol _ => true | _ => false

implement
is_opr(tok) = 
  case+ tok of | TOKopr _ => true | _ => false

implement
is_cpr(tok) = 
  case+ tok of | TOKcpr _ => true | _ => false

implement
is_osq(tok) = 
  case+ tok of | TOKosq _ => true | _ => false

implement
is_csq(tok) = 
  case+ tok of | TOKcsq _ => true | _ => false

implement
is_spc(tok) = 
  case+ tok of | TOKspc _ => true | _ => false

implement
is_war(tok) = 
  case+ tok of | TOKwar _ => true | _ => false

implement
is_err(tok) = 
  case+ tok of | TOKerr _ => true | _ => false

implement
is_int(tok) = 
  case+ tok of | TOKint _ => true | _ => false

implement
is_ide(tok) = 
  case+ tok of | TOKide _ => true | _ => false

implement
is_sco(tok) = 
  case+ tok of | TOKsco _ => true | _ => false

implement
is_nwl(tok) = 
  case+ tok of | TOKchr x => x = '\n' | _ => false

//


implement
tok_get_len(tok) =
  case+ tok of 
  | TOKide ide => g1int2int_ssize_int(length(ide))
  | TOKint itn => g1int2int_ssize_int(length(itn))
  | TOKwar war => g1int2int_ssize_int(length(war))
  | TOKerr err => g1int2int_ssize_int(length(err))
  | TOKs2e s2e => g1int2int_ssize_int(length(s2e))
  | TOKnil() => 0
  | _ => 1



fn
tok_get_len_free(tok: token): int = let
  val xs = 
  (
    case+ tok of 
    | TOKide ide => g1int2int_ssize_int(length(ide))
    | TOKint itn => g1int2int_ssize_int(length(itn))
    | TOKwar war => g1int2int_ssize_int(length(war))
    | TOKerr err => g1int2int_ssize_int(length(err))
    | TOKs2e s2e => g1int2int_ssize_int(length(s2e))
    | TOKnil() => 0
    | _ => 1
  )
  val () = free_token(tok)
in
  xs
end

(* ****** ****** *)

(* end of [token_lib.dats] *)