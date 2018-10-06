staload "./vtypes.sats"


(* ****** ****** *)

datavtype errkind =
  | ERRwarn   of toks // warning message
  | ERRparse  of toks // error(parsing)
  | ERRtyleq  of toks // mismatch of static terms (tyleq)
  | ERRdynexp of toks // 'the dynamic expression cannot be assigned the type
  | ERRdynvar of toks // 'the (non-linear) dynamic variable [xs$1123(-1)] is not avail
  | ERRcstpat of toks // 'the constructor pattern cannot .... [S2Eerrexp()]
  | ERRunsolv of toks // unsolved constraint C3NSTRprop(...)
  | ERRother  of toks // undefined
  | ERRlexing of toks // error(lexing): ...
  | ERRshow   of toks // error(parsing): ...
  | ERRexit2  of toks // exit(2) ...
  | ERRsymbol of toks 
  | ERRsorts  of toks // mismatch of sorts
  | ERRsortu  of toks // mismatch of sorts in unification
  | ERRnonex  of toks 
  | ERRfound  of toks 
  | ERRlast   of toktup // the summary i.e. "exit(ATS): ..."
  | ERRunit   of () 
//(*
  | ERRsimpre of toks  // the static expression needs to be impredicative 
                         // but it is assigned the sort [...].
//*)
    
(*

  | ERRnot_ascribed of toks
example ::
//
fn
print_toks_tokens2(xs: !toks) = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env)
    = (fprint_token(stdout_ref, c); nl)
in
  list_vt_foreach(xs)
end

// ^^^^^^^ // below is the message

error(2): 
the dynamic expression is expected to be ascribed a type but it is not.
*)

(*
  | ERRlindynexp of toks // 'the linear dynamic expression cannot be assigned the type
  | ERRnonlindynexp of toks // 'the (non-linear) dynamic expression cannot be assigned the type
*)

(* ****** ****** *)


vtypedef errtup 
  = @(toks, toks, errkind)

vtypedef errtups
  = List0_vt(errtup)


(* ****** ****** *)
