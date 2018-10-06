(* ****** ****** *)


#ifndef TOKENS_NONE

  #define TOKENS_NONE

  #include "share/atspre_staload.hats"
  #staload UN = "prelude/SATS/unsafe.sats"
  #include "./token.dats"

#endif

(* ****** ****** *)

(* staload UN = "prelude/SATS/unsafe.sats" *)
staload "./../SATS/token.sats"



extern 
fun 
tokenize
(cs: stream_vt(char)): stream_vt(token)


(* ****** ****** *)


fn iscol(chr: char): bool = chr = ':'
fn issco(chr: char): bool = chr = ';'
fn isosq(chr: char): bool = chr = '\['
fn iscsq(chr: char): bool = chr = ']'
fn isopr(chr: char): bool = chr = '\('
fn iscpr(chr: char): bool = chr = ')'
fn isspc(chr: char): bool = chr = ' '
fn isuscore(chr: char): bool = chr = '_'

(* ****** ****** *)


local

macdef prelude_string_make_rlist = string_make_rlist

fun 
aux1
(c0: char,cs: stream_vt(char)) : stream_vt_con(token) = 
// let val () = print!(c0) in
(
  ifcase
  | isalpha(c0) => aux1_ide(cs, list_vt_sing(c0))
  | isdigit(c0) => aux1_int(cs, list_vt_sing(c0))
  | iscol(c0) => stream_vt_cons(TOKcol(c0), tokenize(cs))
  | issco(c0) => stream_vt_cons(TOKsco(c0), tokenize(cs))
  | isopr(c0) => stream_vt_cons(TOKopr(c0), tokenize(cs))
  | iscpr(c0) => stream_vt_cons(TOKcpr(c0), tokenize(cs))
  | isosq(c0) => stream_vt_cons(TOKosq(c0), tokenize(cs))
  | iscsq(c0) => stream_vt_cons(TOKcsq(c0), tokenize(cs))
  | isspc(c0) => stream_vt_cons(TOKspc(c0), tokenize(cs))
  | _(* else *) => stream_vt_cons(TOKchr(c0), tokenize(cs))
)
//end

and
string_make_rlist0
(cs: List0(char)): Strnptr1 = let
  val cs = $UN.cast{List0(charNZ)}(cs)
  val str = $effmask_wrt(prelude_string_make_rlist(cs))
in
  str
end


and
aux1_ide
(cs:stream_vt(char),ds: List0_vt(char)) : stream_vt_con(token) = 
  case+ !cs of
  | ~stream_vt_nil() => let
      val xs = $UN.list_vt2t(ds)
      val ide = string_make_rlist0(xs)
      val () = free(ds)
    in
      stream_vt_cons(TOKide(ide), stream_vt_make_nil())
    end 
  | ~stream_vt_cons(c0, cs) => 
    ifcase
    | isalnum(c0) => aux1_ide(cs, list_vt_cons(c0, ds))
    | isuscore(c0) => aux1_ide(cs, list_vt_cons(c0, ds))
    | _ => let
        val xs = $UN.list_vt2t(ds)
        val ide = string_make_rlist0(xs)
        val () = free(ds)
        val strcpy = strnptr_copy(ide) 
        val ide0 = $UN.strnptr2string(strcpy)
        val s2e = strstr(ide0, "S2") = 0
        val err = strstr(ide0, "error") = 0
        val wrn = strstr(ide0, "warning") = 0
        val () = strnptr_free(strcpy)
      in
        ifcase
        | s2e => stream_vt_cons(TOKs2e(ide), $ldelay(aux1(c0, cs), ~cs))
        | err => stream_vt_cons(TOKerr(ide), $ldelay(aux1(c0, cs), ~cs))
        | wrn => stream_vt_cons(TOKwar(ide), $ldelay(aux1(c0, cs), ~cs))
        | _ =>   stream_vt_cons(TOKide(ide), $ldelay(aux1(c0, cs), ~cs))
      end

and
aux1_int
(cs:stream_vt(char),ds: List0_vt(char)) : stream_vt_con(token) =
(
  case+ !cs of
  | ~stream_vt_nil() => let
      val xs = $UN.list_vt2t(ds)
      val ide0 = string_make_rlist0(xs)
      val () = free(ds)
    in
      stream_vt_cons{token}(TOKide(ide0), stream_vt_make_nil())
    end 
  | ~stream_vt_cons(c0, cs) =>
    ifcase
    | isalpha(c0) => let
        val xs = $UN.list_vt2t(ds)
        val ide0 = string_make_rlist0(xs)
        val () = free(ds)
      in
        stream_vt_cons
        (TOKint(ide0), $ldelay(aux1_ide(cs, list_vt_sing(c0)), ~cs))
      end
    | isdigit(c0) => aux1_int(cs, list_vt_cons(c0, ds))
    | _ => let
        val xs = $UN.list_vt2t(ds)
        val ide0 = string_make_rlist0(xs)
        val () = free(ds)
      in
        stream_vt_cons(TOKint(ide0), $ldelay(aux1(c0, cs), ~cs))
      end
)

in
  implement
  tokenize(cs) 
    = $ldelay 
    (
      case+ !cs of
      | ~stream_vt_nil() => stream_vt_nil()
      | ~stream_vt_cons(c0, cs) => aux1(c0, cs), lazy_vt_free(cs)
    )
end // end of [local]


(* ****** ****** *)

(* end of [Tokenizer.dats] *)
