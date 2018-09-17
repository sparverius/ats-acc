staload "./token.sats"
staload "./vtypes.sats"

(* ****** ****** *)

(**
  consumes the list 'xs'
*)
fn 
free_toks
(xs: toks): void 
(* overload free with free_toks of 1 *)

(**
  consumes the tuple of token lists
*)
fn
free_toktup
(xs: toktup): void 

(**
  consumes the list of toktups
*)
fn 
free_tokstup
(xs: tokstup): void 

(* ****** ****** *)

fn
printall
(xs: !toks): void 

fn
print_token0
(x: !token): void 

fn
print_token0_free
(x: token): void

fn
print_token0_free_nl
(x: token): void

fn
print_toks_all
(xs: toks): void

fn
print_toks
(xs: !toks): void 

fn
print_toks_tokens
(xs: !toks): void 

fn
print_toks_no_newline
(xs: !toks): void 

fn
print_toks_token
(xs: !toks): void 

fn
print_toks_delim
(xs: !toks, delim: string): void 

fn
print_tokens_free
(xs: toks): void 

fn
print_toks_free_base
(xs: toks): void 

fn
print_toks_free
(xs: toks): void 

fn
print_toks_free_nonewln
(xs: toks): void 

fn
print_toks_delim_free
(xs: toks, delim: string): void 

(* ****** ****** *)

(**
  the character length of the list of tokens
*)
fn
toks_get_len
(tok: !toks): int 

(**
  a copy of the given list 'xs'
*)
fn
toks_copy
(xs: !toks): toks 

(**
  checks if two token lists are equal
*)
fn
toks_eq_toks
(xs: !toks, ys: !toks): bool 

(* ****** ****** *)

// '*_free' means that the argument (a linear list) will be consumed

(* ****** ****** *)

(**
  accumulate a list until the condition 'pred' is true 
*)
fn 
take_until_free
(xs: toks, pred: !(!token) -> bool): toks 

(**
  accumulate a list until after the condition 'pred' is true 
*)
fn 
take_until_free2
(xs: toks, pred: !(!token) -> bool): toks 

(**
  accumulate a list WHILE after the condition 'pred' is true 
*)
fn 
take_while_free
(xs: toks, pred: !(!token) -> bool): toks 

(**
  more specifically, splits a list (toks) into two separate lists when the
  condition 'pred' is met
*)
fun
takeskip_until_free
(toks, pred: !(!token) -> bool): (toks, toks)


(*
(**
  splits a list (toks) into two separate lists when the
  condition 'pred' is met (by accumulating the 'take' and returning the rest)
*)
fn 
takeskip_until_free
(xs: toks, pred: !(!token) -> bool): @(toks, toks) 
*)

fn 
takeskip_until_in_free
(xs: toks, pred: !(!token) -> bool): (toks, toks) 

(**
  drop tokens fromm list WHILE the condition 'pred' is true
  returns rest of list 
*)
fn 
skip_while_free
(xs: toks, pred: !(!token) -> bool): toks 

(**
  drop tokens fromm list until the condition 'pred' is true
  returns rest of list 
*)
fn 
skip_until_free
(xs: toks, pred: !(!token) -> bool): toks 

(**
  drop tokens fromm list until the condition 'pred' is true
  returns rest of list -- inclusive (takes the predicate token as well)
*)
fn 
skip_until_in_free
(xs: toks, pred: !(!token) -> bool): toks 

(**
  returns a copy of the lists head
  - does not free the list 'xs'
*)
fn 
toks_head
(xs: !toks): token 

(**
  returns a copy of the lists head (and frees the list)
*)
fn
toks_head_free
(xs: toks): token 

(**
  returns a tuple of the head of the list and rest
*)
fn
toks_head_tail_free
(xs: toks): (token, toks) 

fn
print_toks_free_nonewln_skip1
(xs: toks): void 

(**
  returns a (linear) option of the remaining list
*)
fn 
drop_opt
(xs: !toks, n: int): Option_vt(toks) 

(**
  returns a (linear) option of the remaining list
  also, frees the list 'xs'
*)
fn 
drop_opt_free
(xs: toks, n: int): Option_vt(toks) 

(**
  determines if the token at a given location (n) meets the condition 'pred'
*)
fn 
token_is_pred
(xs: !toks, n: int, pred: !(!token) -> bool): bool 

(**
  returns the 'nth' token of the list
*)
fn 
get_token_at
(xs: !toks, n: int): Option_vt(token) 

(**
  returns the 'nth' token of the list
  if the list is empty returns the empty token (TOKnil)
*)
fn 
get_token_at_unsafe
(xs: !toks, n: int): token 

(**
  returns the 'nth' ide token of the list
*)
fn 
get_token_n_ide
(xs: !toks, n: int): token 

(**
  similar to drop opt
  does not however throw exception
*)
fn
drop_exn
(xs: !toks, n: int): toks 

(**
  drop the first element of the list
  returns a tuple of the first element and the rest of list
*)
fn
drop_head_tup
(xs: toks): (token, toks) 

(**
  same as drop_exn 
  except frees the list xs
  consumes 'xs'
*)
fn
drop_exn_free
(xs: toks, n: int): toks 

(**
  retains xs
*)
fn
drophead_opt
(xs: !toks, n: int): Option_vt(token) 

(**
  retains xs
*)
fn
toks_pred0_is_pred1
(xs: !toks, pred0: !(!token) -> bool, pred1: !(!token) -> bool, n: int): bool

(**
  true if the first element of the list 'xs' satisfies the condition 'pred'
  retains 'xs'
*)
fn 
head_is_pred
(xs: !toks, pred: !(!token) -> bool): bool 

(**
  same as 'head_is_pred' except frees the list 'xs'
  consumes 'xs'
*)
fn 
head_is_pred_free
(xs: toks, pred: !(!token) -> bool): bool 

(**
  given a list 'xs', 
  ot (test for open token function)
  ct (test for close token function) (boolean)  
  returns the length of the list meeting these conditions
  retains 'xs'
*)
fn
peek_x_depth
(xs: !toks, ot: !(!token) -> bool, ct: !(!token) -> bool): int 

(**
  returns the length of (.....)
  retains 'xs'
*)
fn
peek_paren_depth
(xs: !toks): int 

(**
  returns the length of [.....]
  retains 'xs'
*)
fn
peek_square_depth
(xs: !toks): int 

(**
  returns '(...)' and rest of list after 
  consumes 'xs'
*)
fn
peek_paren_list
(xs: toks): (toks, toks) 

(**
  should probably be named 'pop' instead
  assumes list starts after a paren
  returns '...)' and rest of list after 
  consumes 'xs'
*)
fn
peek_paren_list2
(xs: toks): (toks, toks) 

(**
  assumes list starts after a '['
  returns '...]' and rest of list after 
  consumes 'xs'
*)
fn
peek_square_list_osq
(xs: toks): (toks, toks) 

(**
  assumes list starts after a '('
  returns '...)' and rest of list after 
  consumes 'xs'
*)
fn
peek_paren_list_opr
(xs: toks): (toks, toks) 

(**
  returns the inside of parens ('...') and rest of list
  consumes 'xs'
*)
fn
peek_paren_list3
(xs: toks): (toks, toks) 
