#ifndef TOKENS_NONE

#include "./token.dats"
staload UN = "prelude/SATS/unsafe.sats"

#endif

(*
#include "./token.dats"
staload UN = "prelude/SATS/unsafe.sats"
*)

(*
datavtype token =
  | TOKide of Strnptr // ide=alpha[alnum]*
  | TOKint of Strnptr // int=digit[digit]*
  | TOKchr of (char)  // special character
  (* | TOKwar of Strnptr // warning *)
  (* | TOKerr of Strnptr // error *)
  (* | TOKs2e of Strnptr // S2E... *)
  (* | TOKcol of (char) // : colon *)
  (* | TOKsco of (char) // ; semi-colon *)
  (* | TOKopr of (char) // ( open paren *)
  (* | TOKcpr of (char) // ) close paren *)
  (* | TOKosq of (char) // [ open square bracket *)
  (* | TOKcsq of (char) // ] close square bracket *)
  (* | TOKspc of (char) // ' ' *)
  (* | TOKnil of () // ' '   *)
*)

extern
fun
{a:vt@ype}
{b:type}
eqeq_tok(x: a, y: b): bool


fn 
tok_is_ide
(x: !token): bool =
  case+ x of | TOKide _ => true | _ => false
fn
tok_is_col
(x: !token): bool =
  case+ x of | TOKcol _ => true | _ => false
fn
tok_is_s2e
(x: !token): bool =
  case+ x of | TOKs2e _ => true | _ => false

//

fn
strnptr_eq_string
(i: !Strnptr, s: string): bool = let
  val x0 = strnptr_copy(i)
  val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
  val x1 = $UN.strnptr2string(x0)
  val res = x1 = s
  val () = strnptr_free(x0)
in
  res
end

overload = with strnptr_eq_string of 10

//

fn
tok_ide_eq(x: !token, s: string): bool =
  case+ x of | TOKide i => strnptr_eq_string(i, s) | _ => false
fn
tok_int_eq(x: !token, s: string): bool =
  case+ x of | TOKint i => strnptr_eq_string(i, s) | _ => false
fn
tok_s2e_eq(x: !token, s: string): bool =
  case+ x of | TOKs2e i => strnptr_eq_string(i, s) | _ => false

fn 
tok_chr_eq(x: !token, c: char): bool =
  case+ x of | TOKchr i => i = c | _ => false

//

fn 
tok_chr_eq_int(x: !token, c: int): bool =
  case+ x of | TOKchr chr => char2int0(chr) = c | _ => false

overload tokeq with tok_chr_eq
overload = with tok_chr_eq

//

fn
strnptr_eq_strnptr
(i: !Strnptr, j: !Strnptr): bool = let
  val x0 = strnptr_copy(i)
  val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
  val x1 = $UN.strnptr2string(x0)

  val y0 = strnptr_copy(j)
  val _ = assertloc(g1int2int_ssize_int(length(y0)) > 0)
  val y1 = $UN.strnptr2string(y0)

  val res = (y1 = x1)
  //val () = println!("\nres = ", res)
  val () = strnptr_free(x0)
  val () = strnptr_free(y0)
in
  res
end


fn
tok_int_eq_tok_int(x: !token, y: !token): bool =
  case+ (x, y) of 
  | (TOKint i, TOKint j) => strnptr_eq_strnptr(i, j)
  | (_, _) => false

//

fn
is_chr(tok: !token): bool = 
  case+ tok of | TOKchr _ => true | _ => false
fn
is_col(tok: !token): bool = 
  case+ tok of | TOKcol _ => true | _ => false
fn
is_opr(tok: !token): bool = 
  case+ tok of | TOKopr _ => true | _ => false
fn
is_cpr(tok: !token): bool = 
  case+ tok of | TOKcpr _ => true | _ => false
fn
is_osq(tok: !token): bool = 
  case+ tok of | TOKosq _ => true | _ => false
fn
is_csq(tok: !token): bool = 
  case+ tok of | TOKcsq _ => true | _ => false
fn
is_spc(tok: !token): bool = 
  case+ tok of | TOKspc _ => true | _ => false
fn
is_war(tok: !token): bool = 
  case+ tok of | TOKwar _ => true | _ => false
fn
is_err(tok: !token): bool = 
  case+ tok of | TOKerr _ => true | _ => false
fn
is_int(tok: !token): bool = 
  case+ tok of | TOKint _ => true | _ => false
fn
is_ide(tok: !token): bool = 
  case+ tok of | TOKide _ => true | _ => false
fn
is_sco(tok: !token): bool = 
  case+ tok of | TOKsco _ => true | _ => false
fn
is_nwl(tok: !token): bool = 
  case+ tok of | TOKchr x => x = '\n' | _ => false

//

fn
tok_get_len
(tok: !token): int =
  case+ tok of 
  | TOKide ide => g1int2int_ssize_int(length(ide))
  | TOKint itn => g1int2int_ssize_int(length(itn))
  | TOKwar war => g1int2int_ssize_int(length(war))
  | TOKerr err => g1int2int_ssize_int(length(err))
  | TOKs2e s2e => g1int2int_ssize_int(length(s2e))
  | TOKnil() => 0
  | _ => 1

