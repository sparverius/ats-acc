staload "./token.sats"

(* ****** ****** *)


(**
  true if token is an identifier
*)
fn 
tok_is_ide
(x: !token): bool 

(**
  true if token is a ':'
*)
fn
tok_is_col
(x: !token): bool 

(**
  true if token is a S2* tree
*)
fn
tok_is_s2e
(x: !token): bool 


(* ****** ****** *)


(**
  compares contents of a string pointer with a string
*)
fn
strnptr_eq_string
(i: !Strnptr, s: string): bool 
  overload = with strnptr_eq_string of 10


(* ****** ****** *)


(**
  compares the value of the identifier token with a string
*)
fn
tok_ide_eq(x: !token, s: string): bool 

(**
  compares the value of the int token with a string
*)
fn
tok_int_eq(x: !token, s: string): bool 

(**
  compares the value of the character token with a string
*)
fn 
tok_chr_eq(x: !token, c: char): bool 

(**
  compares the value of the S2E abstract syntax tree with a string
*)
fn
tok_s2e_eq(x: !token, s: string): bool 


(* ****** ****** *)

(**
  compares the integer representation of the character token with an integer
*)
fn 
tok_chr_eq_int(x: !token, c: int): bool 
  overload tokeq with tok_chr_eq
  overload = with tok_chr_eq

(* ****** ****** *)

(**
  checks if two string-pointers are equal
*)
fn
strnptr_eq_strnptr
(i: !Strnptr, j: !Strnptr): bool 

(* ****** ****** *)

(**
  compares the integer value of two integer tokens
*)
fn
tok_int_eq_tok_int(x: !token, y: !token): bool 

(* ****** ****** *)

fn
is_chr(tok: !token): bool // single character

fn
is_col(tok: !token): bool // ':'

fn
is_opr(tok: !token): bool // '('

fn
is_cpr(tok: !token): bool // ')'

fn
is_osq(tok: !token): bool // '['

fn
is_csq(tok: !token): bool // ']'

fn
is_spc(tok: !token): bool // ' '

fn
is_war(tok: !token): bool // warning

fn
is_err(tok: !token): bool // error

fn
is_int(tok: !token): bool // digit[digit]*

fn
is_ide(tok: !token): bool // alpha[alnum]*

fn
is_sco(tok: !token): bool // ';'

fn
is_nwl(tok: !token): bool // '\n'


(* ****** ****** *)

(**
  returns the length of a token
*)

fn
tok_get_len
(tok: !token): int 


(* ****** ****** *)

(* end of [token_lib.sats] *)  