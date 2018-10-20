#ifndef TOKENS_NONE

  staload "./token.sats"
  staload "./vtypes.sats"
#endif 

(* ****** ****** *)

// helpers

fn(*{}*)
get_simplified_idestring
(idestring: string, verbose: bool): string

fn(*{}*)
print_simplified_idestring
(idestring: string, color_name: string, color: bool, verbose: bool): void

fn(*{}*)
just_print
(x0: token, color: bool, color_name: string, verbose: bool): void

fn(*{}*)
simplify_whats_inside
(xs0: toks, verbose: bool): (toks, toks)

fn(*{}*)
simplify_idestring
(x0: token, color: bool, verbose: bool): void

fn(*{}*)
generic_simplify
(x0: token, xs0: toks, insert: string, verbose: bool): void


(* ****** ****** *) // print simplified S2E

(* 
from *** ATS2/src/pats_staexp2.sats *** (line 477-557 and ~559-~582) 
datatype s2exp_node
*)

(*
| S2Eint of int // integer
*)
fn(*{}*)
simplify_S2Eint
(x0: token, xs0: toks, verbose: bool): void
(*
| S2Eintinf of intinf // integer of flex precision
*)
fn(*{}*)
simplify_S2Eintinf
(x0: token, xs0: toks, verbose: bool): void
(*
| S2Efloat of string // static floating-points
*)
fn(*{}*) 
simplify_S2Efloat
(x0: token, xs0: toks, verbose: bool): void
(*
| S2Estring of string // static string constants
*)
fn(*{}*) 
simplify_S2Estring
(x0: token, xs0: toks, verbose: bool): void
(*
| S2Ecst of s2cst // constant
*)
fn(*{}*)
simplify_S2Ecst
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(* 
helper
*)  
fn(*{}*)
simplify_S2Eext
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2Eextype of (string(*name*), s2explstlst) // external type
*)
fn(*{}*)
simplify_S2Eextype
(x0: token, xs0: toks, verbose: bool): toks //= simplify_S2ext(x0, xs0)
(*
| S2Eextkind of (string(*name*), s2explstlst) // external tkind
*)
fn(*{}*)
simplify_S2Eextkind
(x0: token, xs0: toks, verbose: bool): toks //= simplify_S2ext(x0, xs0)
(*
| S2Evar of s2var // universal variable
*)
fn(*{}*)
simplify_S2Evar
(x0: token, xs0: toks, verbose: bool): (toks, toks, toks)
(*
| S2EVar of s2Var // existential variable
*)
fn(*{}*) 
simplify_S2EVar
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2Ehole of s2hole // it used to form contexts
*)
fn(*{}*)
simplify_S2Ehole
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Edatcontyp of (* unfolded datatype *)
    (d2con, s2explst) (* constructor and types of arguments *)
*)
fn(*{}*)
simplify_S2Edatcontyp
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Edatconptr of (* unfolded datatype *)
    (d2con, s2exp, s2explst) (* constructor and addrs of arguments *)
*)
fn(*{}*)
simplify_S2Edatconptr
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Eat of (s2exp, s2exp) // for at-views
*)
fn(*{}*)
simplify_S2Eat
(x0: token, xs0: toks, verbose: bool): toks 
(*
| S2Esizeof of (s2exp) // for sizes of types
*)
fn(*{}*)
simplify_sizeof
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2Eeff of (s2eff) // effects
*)
fn(*{}*)
simplify_S2Eeff
(x0: token, xs0: toks, verbose: bool): toks

//
(*
| S2Eeqeq of (s2exp, s2exp) // generic static equality
*)
fn(*{}*)
simplify_S2Eeqeq
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Eproj of (s2exp(*addr*), s2exp(*type*), s2lablst) // projection
*)
fn(*{}*)
simplify_S2Eproj
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Eapp of (s2exp, s2explst) // static application
*)
fn(*{}*)
simplify_S2Eapp
(x0: token, xs0: toks, verbose: bool): (toks, toks, toks, toks, toks)
(*
| S2Elam of (s2varlst, s2exp) // static abstraction
*)
fn(*{}*)
simplify_S2Elam
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Efun of 
  ( // function type
    funclo, int(*lin*), s2eff, int(*npf*), s2explst(*arg*), s2exp(*res*))
*)
fn(*{}*)
simplify_S2Efun
(x0: token, xs: toks, verbose: bool): (toks, toks, toks, toks, toks)
(*
| S2Emetfun of (stampopt, s2explst, s2exp) // metricked function
*)
fn(*{}*)
simplify_S2Emetfun
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Emetdec of (s2explst(*met*), s2explst(*metbound*)) // strictly decreasing
*)
fn(*{}*)
simplify_S2Emetdec
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Etop of (int(*knd*), s2exp) // knd: 0/1: topization/typization
*)
fn(*{}*)
simplify_S2Etop
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Ewithout of (s2exp) // for a component taken out by the [view@] operation
*)
fn(*{}*)
simplify_S2Ewithout
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Etyarr of (s2exp (*element*), s2explst (*dimension*))
*)
fn(*{}*)
simplify_S2Etyarr
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Einvar of (s2exp) // it is a special type for handling type unification
        // HX: note that [S2Einvar] is *not* related to [S1Einvar];
*)
fn(*{}*)
simplify_S2Einvar
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2Etyrec of (tyreckind, int(*npf*), labs2explst) // tuple and record
*)
fn(*{}*) 
simplify_S2Etyrec
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2Eexi of ( // exist. quantified type
          s2varlst(*vars*), s2explst(*props*), s2exp(*body*) )
*)
fn(*{}*)
simplify_S2Eexi
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Euni of ( // universally quantified type
         s2varlst(*vars*), s2explst(*props*), s2exp(*body*))
*)
fn(*{}*)
simplify_S2Euni
(x0: token, xs0: toks, verbose: bool): (toks, toks, toks)
(*
| S2Erefarg of (int(*0/1:val/ref*), s2exp) (* !/&: call-by-val/ref *)
*)
fn(*{}*)
simplify_S2Erefarg
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Evararg of (s2exp) // variadic argument type
*)
fn(*{}*)
simplify_S2Evararg
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Ewthtype of (s2exp, wths2explst) // the result part of a fun type
*)
fn(*{}*)
simplify_S2Ewthtype
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2Eerrexp of ((*void*)) 
            // HX: placeholder for indicating error or something else
*)
fn(*{}*)
simplify_S2Eerrexp
(x0: token, xs0: toks, verbose: bool): (toks, toks)
    // see simplify


(* end of datatype s2exp_node *)


(* ****** ****** *)


(* datatype s2rtbas *)  

(*
| S2RTBASpre of (symbol) // predicative: int, bool, ...
*)
fn(*{}*)
simplify_S2RTBASpre
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2RTBASimp of (int(*knd*), symbol) // impredicative sorts
*)
fn(*{}*)
simplify_S2RTBASimp
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2RTBASdef of (s2rtdat) // user-defined datasorts
*)
fn(*{}*)
simplify_S2RTBASdef
(x0: token, xs0: toks, verbose: bool): (toks, toks)


(* ****** ****** *)

(* 
datatype s2rt 
*)
(*
| S2RTfun of (s2rtlst, s2rt) // function sort
*)
fn(*{}*)
simplify_S2RTfun
(x0: token, xs0: toks, verbose: bool): (toks, toks)
(*
| S2RTtup of s2rtlst (* tuple sort *)
*)
fn(*{}*)
simplify_S2RTtup
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2RTVar of s2rtVar // HX: unification variable
*)
fn(*{}*)
simplify_S2RTVar
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2RTerr of ((*void*)) // HX: error indication
*)
fn(*{}*)
simplify_S2RTerr
(x0: token, xs0: toks, verbose: bool): toks
(*
| S2RTbas of s2rtbas (* base sort *)
*)
fn(*{}*)
simplify_S2RTbas
(x0: token, xs0: toks, verbose: bool): (toks, toks)

(* ****** ****** *) // begin Simplify Print

fn(*{}*)
simplify_print
(xs: toks, color: bool, verbose: bool): void 
