staload "./vtypes.sats"
staload "./errkind.sats"

fun
print_errkind: print_vtype(errkind)
fun
fprint_errkind: fprint_vtype(errkind)

overload print with print_errkind
overload fprint with fprint_errkind


(* ****** ****** *)


fn 
free_errkind
(x : errkind): void

fn
get_errkind_string
(xs: !errkind): string 


(* ****** ****** *)


fun{} 
print_parse(xs: toks, color: bool): void

fun{}
print_other(xs: toks, color: bool): void

fun{}
print_tyleq(xs: toks, color: bool): void

fun{}
print_exit2(xs: toks, color: bool): void

fun{}
print_found(xs: toks, color: bool): void

fun{}
print_sorts(xs: toks, color: bool): void

fun{}
print_sortu(xs: toks, color: bool): void

fun{}
print_nonex(xs: toks, color: bool): void

fun{}
print_other(xs: toks, color: bool): void

fun{}
print_dynexp(xs: toks, color: bool): void

fun{}
print_dynvar(xs: toks, color: bool): void

fun{}
print_cstpat(xs: toks, color: bool): void

fun{}
print_unsolv(xs: toks, color: bool): void

fun{}
print_lexing(xs: toks, color: bool): void

fun{}
print_symbol(xs: toks, color: bool): void

fun{}
print_show(xs: toks, color: bool): void

fun{}
print_warn(xs: toks, color: bool): void

fun{}
print_unit((* *)): void

fun{}
print_last(xs: toktup, color: bool): void

//(*
fun{}
print_simpre(xs: toks, color: bool): void
//*)

(* ****** ****** *)


fun{}
print_errkind_single
(x2: errkind, color: bool): void 


(* ****** ****** *)

fn
free_errtup
(xs: errtup): void 


fn 
free_errtups
(xs: errtups): void 

(* ****** ****** *)