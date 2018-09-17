(* ****** ****** *)

#ifndef TOKENS_NONE

//(*
#include "./token.dats"
staload UN = "prelude/SATS/unsafe.sats"
#include "./token_lib.dats"
//*)


#endif




(* ****** ****** *)

vtypedef tok_vt = List0_vt(token)
vtypedef toks = List0_vt(token)


(* ****** ****** *)


fn
printall
(xs: !toks): void = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) = fprint_token(stdout_ref, c)
in
  list_vt_foreach(xs)
end

fn
print_token0
(x: !token): void =
  case+ x of
  | TOKide ide => print ide
  | TOKint itn => print itn
  | TOKchr chr => print chr
  | TOKs2e s2e => print s2e
  | TOKcol col => print col
  | TOKsco sco => print sco
  | TOKopr opr => print opr
  | TOKcpr cpr => print cpr
  | TOKosq osq => print osq
  | TOKcsq csq => print csq
  | TOKspc spc => print spc
  | TOKwar war => print war
  | TOKerr err => print err
  | TOKnil _ => print "nil"


(*
fn
print_token0 // print_token1
(x: !token): void =
  case+ x of
  | TOKide ide => print ide
  | TOKint itn => print itn
  | TOKchr chr => print chr
  | TOKs2e s2e => (prcc; print s2e; prcc) //print_esc(light_yellow, s2e)
  | TOKcol col => (prc blue; print col; prcc)
  | TOKsco sco => print sco
  | TOKopr opr => print opr
  | TOKcpr cpr => print cpr
  | TOKosq osq => print osq
  | TOKcsq csq => print csq
  | TOKspc spc => print spc
  | TOKwar war => print_esc(light_red, war) //print war
  | TOKerr err => print_esc(red, err) //print err
  | TOKnil => print "nil"
*)


fn
print_token0_free
(x: token): void =
  case+ x of
  | ~TOKide ide => (print ide; strnptr_free(ide))
  | ~TOKint itn => (print itn; strnptr_free(itn))
  | ~TOKs2e s2e => (print s2e; strnptr_free(s2e))
  | ~TOKwar war => (print war; strnptr_free(war))
  | ~TOKerr err => (print err; strnptr_free(err))
  | ~TOKchr chr => (print chr)  //(if (chr != '\n') then print chr) 
  | ~TOKcol col => (print col) 
  | ~TOKsco sco => (print sco) 
  | ~TOKopr opr => (print opr) 
  | ~TOKcpr cpr => (print cpr) 
  | ~TOKosq osq => (print osq) 
  | ~TOKcsq csq => (print csq) 
  | ~TOKspc spc => (print spc) 
  | ~TOKnil _ => print "nil"


fn
print_token0_free // no newline different than above
(x: token): void =
  case+ x of
  | ~TOKide ide => (print ide; strnptr_free(ide))
  | ~TOKint itn => (print itn; strnptr_free(itn))
  | ~TOKs2e s2e => (print s2e; strnptr_free(s2e))
  | ~TOKwar war => (print war; strnptr_free(war))
  | ~TOKerr err => (print err; strnptr_free(err))
  | ~TOKchr chr => (if (chr != '\n') then print chr) 
  | ~TOKcol col => (print col) 
  | ~TOKsco sco => (print sco) 
  | ~TOKopr opr => (print opr) 
  | ~TOKcpr cpr => (print cpr) 
  | ~TOKosq osq => (print osq) 
  | ~TOKcsq csq => (print csq) 
  | ~TOKspc spc => (print spc) 
  | ~TOKnil _ => print "nil"


fn
print_toks // THIS ONE IS THE ORIG
(xs: !toks): void = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = print_token0(c)
in
  list_vt_foreach(xs)
end


fn
print_toks_tokens
(xs: !toks): void = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = fprint_token(stdout_ref, c)
in
  list_vt_foreach(xs)
end


fn
print_toks_no_newline
(xs: !toks): void = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (if not(is_nwl(c)) then print_token0(c))
in
  list_vt_foreach(xs)
end


fn
print_toks_token
(xs: !tok_vt): void = let 
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (fprint_token(stdout_ref, c); fprint(stdout_ref, ", "))
in
  list_vt_foreach(xs); print "\n"
end


fn
print_toks_delim
(xs: !tok_vt, delim: string): void = let 
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (fprint_token(stdout_ref, c); fprint(stdout_ref, delim))
in
  list_vt_foreach(xs); print "\n"
end


fn
print_tokens_free
(xs: tok_vt): void = let
  fun
  auxmain (xs: tok_vt): void = 
    case+ xs of
    | ~nil_vt() => print "\n"
    | ~cons_vt(x, xs) => ( 
        fprint_token(stdout_ref, x); 
        free_token(x);
        fprint(stdout_ref, ", ");
        auxmain(xs)
      )
in
  auxmain(xs)
end


fn
print_toks_free
(xs: tok_vt): void = let
  fun
  auxmain (xs: tok_vt): void = 
    case+ xs of
    | ~nil_vt() => print "\n"
    | ~cons_vt(x, xs) => ( 
        (* fprint_token(stdout_ref, x);  *)
        print_token0_free(x);
        (* free_token(x); *)
        (* fprint(stdout_ref, ", "); *)
        auxmain(xs)
      )
in
  auxmain(xs)
end


fn
print_toks_free_nonewln
(xs: tok_vt): void = let
  fun
  auxmain (xs: tok_vt): void = 
    case+ xs of
    | ~nil_vt() => () //print "\n"
    | ~cons_vt(x, xs) => ( 
        (* fprint_token(stdout_ref, x);  *)
        print_token0_free(x);
        (* free_token(x); *)
        (* fprint(stdout_ref, ", "); *)
        auxmain(xs)
      )
in
  auxmain(xs)
end


fn
print_toks_delim_free
(xs: tok_vt, delim: string): void = let
  fun
  auxmain (xs: tok_vt): void = 
    case+ xs of
    | ~nil_vt() => print "\n"
    | ~cons_vt(x, xs) => ( 
        fprint_token(stdout_ref, x); 
        free_token(x);
        fprint(stdout_ref, delim);
        auxmain(xs)
      )
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
free_toks
(xs: tok_vt): void 
  = list_vt_freelin<token>(xs) where { 
    implement list_vt_freelin$clear<token>(x) = {
      val _ = (
        case+ x of
        | ~TOKide (s) => let val () = strnptr_free(s) in x end
        | ~TOKint (s) => let val () = strnptr_free(s) in x end
        | ~TOKs2e (s) => let val () = strnptr_free(s) in x end
        | ~TOKwar (s) => let val () = strnptr_free(s) in x end
        | ~TOKerr (s) => let val () = strnptr_free(s) in x end
        | ~TOKchr (_) => x
        | ~TOKcol (_) => x
        | ~TOKsco (_) => x
        | ~TOKopr (_) => x
        | ~TOKcpr (_) => x
        | ~TOKosq (_) => x
        | ~TOKcsq (_) => x
        | ~TOKspc (_) => x
        | ~TOKnil _ => x
      )
    }
  }

overload free with free_toks of 1


(* ****** ****** *)


(*
fn
toks_get_len
(tok: !toks): int = let
  macdef sz2i1 = g1int2int_ssize_int
  fun 
  auxmain(xs: !toks, res: int): int =
  case+ xs of 
  | nil_vt() => res
  | cons_vt(x, xs1) => 
    case+ x of 
    | TOKide ide => sz2i1(length(ide))
    | TOKint itn => g1int2int_ssize_int(length(itn))
    | TOKwar war => g1int2int_ssize_int(length(war))
    | TOKerr err => g1int2int_ssize_int(length(err))
    | TOKs2e s2e => g1int2int_ssize_int(length(s2e))
    | TOKnil() => 0
    | _ => 1
in
  auxmain(tok, 0)
end
*)
fn
toks_get_len
(tok: !toks): int = let
  macdef sz2i1 = g1int2int_ssize_int
  fun 
  auxmain(xs: !toks, res: int): int =
  case+ xs of 
  | nil_vt() => res
  | cons_vt(x, xs1) => 
    case+ x of 
    | TOKide ide => auxmain(xs1, sz2i1(length(ide)) + res)
    | TOKint itn => auxmain(xs1, sz2i1(length(itn)) + res)
    | TOKwar war => auxmain(xs1, sz2i1(length(war)) + res)
    | TOKerr err => auxmain(xs1, sz2i1(length(err)) + res)
    | TOKs2e s2e => auxmain(xs1, sz2i1(length(s2e)) + res)
    | TOKnil() => auxmain(xs1, res)
    | _ => auxmain(xs1, res + 1)
in
  auxmain(tok, 0)
end

(* ****** ****** *)


fn
toks_copy
(xs: !tok_vt): tok_vt = let
  fun 
  auxmain (xs: !tok_vt, res: tok_vt): tok_vt = 
    case+ xs of
    | nil_vt() => list_vt_reverse(res)
    | cons_vt(x1, xs1) => let 
        val x2 = create_token(x1) 
      in
        auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn
toks_copy_free
(xs: tok_vt): tok_vt = let
  fun 
  auxmain (xs: tok_vt, res: tok_vt): tok_vt = 
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => let 
        val x2 = create_token(x1)
        val () = free_token(x1)
      in
        auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


typedef cfun(a:t@ype, b:t@ype) = a -<cloref1> b
vtypedef cfun_vt( a: vt@ype, b:t@ype) = a -<cloref1> b


(* ****** ****** *)


fn 
toks_take_until0
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain(xs: !tok_vt, res: tok_vt): tok_vt =
  case+ xs of
  | nil_vt() => list_vt_reverse(res)
  | cons_vt(x1, xs1) => (
    let val x2 = create_token(x1)
    in
      ifcase
      | pred(x1) => list_vt_reverse(cons_vt(x2, res))
      | _ => auxmain(xs1, cons_vt(x2, res))
    end
  )
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


// same as toks_take_until0
fn 
take_until
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain(xs: !tok_vt, res: tok_vt): tok_vt =
    case+ xs of
    | nil_vt() => list_vt_reverse(res)
    | cons_vt(x1, xs1) => let 
        val x2 = create_token(x1)
      in
        ifcase
        | pred(x1) => list_vt_reverse(cons_vt(x2, res))
        | _ => auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
take_until_free
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain(xs: tok_vt, res: tok_vt): tok_vt =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => let 
        (* val x2 = create_token(x1) *)
      in
        ifcase
        | pred(x1) => let 
            val () = free_toks(xs1) 
          in list_vt_reverse(cons_vt(x1, res))
          end
        | _ => auxmain(xs1, cons_vt(x1, res))
      end
in
  auxmain(xs, nil_vt())
end

fn 
take_until_free2
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain(xs: tok_vt, res: tok_vt): tok_vt =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => let 
        (* val x2 = create_token(x1) *)
      in
        ifcase
        | pred(x1) => let 
            val () = free_toks(xs1) 
            val () = free_token(x1)
          in list_vt_reverse(res)
          end
        | _ => auxmain(xs1, cons_vt(x1, res))
      end
in
  auxmain(xs, nil_vt())
end



(* ****** ****** *)


fn 
take_while
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain(xs: !tok_vt, res: tok_vt): tok_vt =
    case+ xs of
    | nil_vt() => list_vt_reverse(res)
    | cons_vt(x1, xs1) => (
        ifcase
        | pred(x1) => let 
            val x2 = create_token(x1)
          in auxmain(xs1, cons_vt(x2, res)) 
          end
        | _ => list_vt_reverse(res)
  )
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)



fn 
take_while_free
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain (xs: tok_vt, res: tok_vt): tok_vt =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => auxmain(xs1, cons_vt(x1, res)) 
      | _ => let
          val () = free_token(x1)
          val () = free_toks(xs1)
        in list_vt_reverse(res)
        end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
takeskip_until
(xs: !tok_vt, pred: !(!token) -> bool): @(tok_vt, tok_vt) = let
  fun 
  auxmain(xs: !tok_vt, res: tok_vt): @(tok_vt, tok_vt) = 
    case+ xs of
    | nil_vt() => (list_vt_reverse(res), nil_vt())
    | cons_vt(x1, xs1) => let 
        val x2 = create_token(x1) 
      in
        ifcase
        | pred(x1) => let 
            val ys = toks_copy(xs1)
            val ys0 = cons_vt(x2, ys)
          in
            (list_vt_reverse(res), ys0)
            (* (list_vt_reverse(cons_vt(x2, res)), ys) *)
          end
        | _ => auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
takeskip_until_in
(xs: !tok_vt, pred: !(!token) -> bool): @(tok_vt, tok_vt) = let
  fun
  auxmain
  (xs: !tok_vt, res: tok_vt): @(tok_vt, tok_vt) = 
    case+ xs of
    (* | nil_vt() => (res, nil_vt()) *)
    | nil_vt() => (list_vt_reverse(res), nil_vt())
    | cons_vt(x1, xs1) => let 
        val x2 = create_token(x1) 
      in
        ifcase
        | pred(x1) => let 
            val ys = toks_copy(xs1) 
          in
            (list_vt_reverse(cons_vt(x2, res)), ys)
          end
        | _ => auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
takeskip_until_free
(xs: tok_vt, pred: !(!token) -> bool): @(tok_vt, tok_vt) = let
  fun 
  auxmain(xs: tok_vt, res: tok_vt): @(tok_vt, tok_vt) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x1, xs1) =>
        ifcase
        | pred(x1) => (list_vt_reverse(res), cons_vt(x1, xs1))
        | _ => auxmain(xs1, cons_vt(x1, res))
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
takeskip_until_in_free
(xs: tok_vt, pred: !(!token) -> bool): (tok_vt, tok_vt) = let
  fun
  auxmain
  (xs: tok_vt, res: tok_vt): (tok_vt, tok_vt) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    (* | ~nil_vt() => (res, nil_vt()) *)
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => (list_vt_reverse(cons_vt(x1, res)), xs1)
      | _ => auxmain(xs1, cons_vt(x1, res))
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


fn 
skip_while
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun
  auxmain (xs: !tok_vt): tok_vt = 
    case+ xs of
    | nil_vt() => nil_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => auxmain(xs1)
      | _ => let 
          val y = create_token(x1)
          val ys = toks_copy(xs1) 
        in
          cons_vt(y, ys)
        end
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
skip_until
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain (xs: !tok_vt): tok_vt = 
    case+ xs of
    | nil_vt() => nil_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => let 
          val ys = toks_copy(xs1) 
          val y1 = create_token(x1)
        in
          cons_vt(y1, ys)
        end
      | _ => auxmain(xs1)
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
skip_while_free
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun
  auxmain (xs: tok_vt): tok_vt = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => let
          val () = free_token(x1)
        in auxmain(xs1)
        end
      | _ => (* let  *)
        (*   val y = create_token(x1) *)
        (*   val ys = toks_copy(xs1)  *)
        (* in *)
          cons_vt(x1, xs1)//, ys)
        (* end *)
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
skip_until_free
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain (xs: tok_vt): tok_vt = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => cons_vt(x1, xs1)
      | _ => let
          val () = free_token(x1)
        in auxmain(xs1)
        end
in
  auxmain(xs)
end



(* ****** ****** *)


fn 
skip_until_in
(xs: !tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain (xs: !tok_vt): tok_vt = 
    case+ xs of
    | nil_vt() => nil_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => let 
          (* val ys = toks_copy(xs) *)
          val ys = toks_copy(xs1)
        in ys
        end
      | _ => auxmain(xs1)
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
skip_until_in_free
(xs: tok_vt, pred: !(!token) -> bool): tok_vt = let
  fun 
  auxmain (xs: tok_vt): tok_vt = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => let
          val () = free_token(x1) 
          in xs1 (* cons_vt(x1, xs1) *)
          end
      | _ => let
          val () = free_token(x1)
        in auxmain(xs1)
        end
in
  auxmain(xs)
end


(* ****** ****** *)


fn 
toks_head
(xs: !tok_vt): token = //Option_vt(token) = 
  case+ xs of
  | nil_vt() => TOKnil()
  | cons_vt(x1, xs1) => let
      val x = create_token(x1)
    in x
    end


(* ****** ****** *)


fn
toks_head_free
(xs: tok_vt): token = //Option_vt(token) = 
  case+ xs of
  | ~nil_vt() => TOKnil()
  | ~cons_vt(x1, xs1) => let
      val x = create_token_free(x1)
      val () = free_toks(xs1)
    in x
    end


(* ****** ****** *)

fn
toks_head_tail_free
(xs: tok_vt): (token, tok_vt) = //Option_vt(token) = 
  case+ xs of
  | ~nil_vt() => (TOKnil(), nil_vt())
  | ~cons_vt(x1, xs1) => (x1, xs1)


(* ****** ****** *)

fn
print_toks_free_nonewln_skip1
(xs: tok_vt): void = let
  val (x, xs) = toks_head_tail_free(xs)
  val () = free_token(x)
  fun
  auxmain (xs: tok_vt): void = 
    case+ xs of
    | ~nil_vt() => () 
    | ~cons_vt(x, xs) => ( 
        print_token0_free(x);
        auxmain(xs)
      )
in
  auxmain(xs)
end




(* ****** ****** *)


fn 
drop_opt
(xs: !tok_vt, n: int): Option_vt(tok_vt) = let
  fun
  auxmain (xs: !tok_vt, i: int): Option_vt(tok_vt) = 
    case+ xs of
    | nil_vt() => None_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => //Some_vt(xs1)
      (
        let
          val ys = toks_copy(xs1)
        in Some_vt(ys)
        end
      )
      | _ => auxmain(xs1, i+1)
in 
  auxmain(xs, 0)
end


(* ****** ****** *)


fn 
drop_opt_free
(xs: tok_vt, n: int): Option_vt(tok_vt) = let
  fun
  auxmain (xs: tok_vt, i: int): Option_vt(tok_vt) = 
    case+ xs of
    | ~nil_vt() => None_vt()
    | ~cons_vt(x1, xs1) => let
        val () = free_token(x1)
      in
        ifcase
        | n - i = 0 => Some_vt(xs1)
        | _ => auxmain(xs1, i+1)
      end
in 
  auxmain(xs, 0)
end


(* ****** ****** *)

fn 
token_is_pred
(xs: !tok_vt, n: int, pred: !(!token) -> bool): bool = let
  fun
  auxmain (xs: !tok_vt, i: int): bool = 
    case+ xs of
    | nil_vt() => false
    | cons_vt(x1, xs1) => let
        (* val () = free_token(x1) *)
      in
        ifcase
        | n - i = 0 => let val () = print_token0(x1) in pred(x1) end
        | _ => auxmain(xs1, i+1)
      end
in 
  auxmain(xs, 0)
end

fn 
get_token_at
(xs: !tok_vt, n: int): Option_vt(token) = let
  fun
  auxmain (xs: !tok_vt, i: int): Option_vt(token) = 
    case+ xs of
    | nil_vt() => None_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => let 
          //val () = print_token0(x1) 
          val y = create_token(x1)
        in Some_vt(y)
        end
      | _ => auxmain(xs1, i+1)
in 
  auxmain(xs, 0)
end

fn 
get_token_at_unsafe
(xs: !tok_vt, n: int): token = let
// unsafe in that it will return TOKnil if error
  fun
  auxmain (xs: !tok_vt, i: int): token = 
    case+ xs of
    | nil_vt() => TOKnil()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => let 
          val y = create_token(x1)
        in y
        end
      | is_spc(x1) => auxmain(xs1, i)
      | _ => auxmain(xs1, i+1)
in 
  auxmain(xs, 0)
end


fn 
get_token_n_ide
(xs: !tok_vt, n: int): token = let
// unsafe in that it will return TOKnil if error
  fun
  auxmain (xs: !tok_vt, i: int): token = 
    case+ xs of
    | nil_vt() => TOKnil()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 && is_ide(x1) => let 
          val y = create_token(x1)
        in y
        end
      | is_ide(x1) => auxmain(xs1, i+1)
      | _ => auxmain(xs1, i)
in 
  auxmain(xs, 0)
end


(* ****** ****** *)


// does not throw exception yet
fn
drop_exn
(xs: !tok_vt, n: int): tok_vt = let
  fun
  auxmain (xs: !tok_vt, i: int): tok_vt = 
    case+ xs of
    | nil_vt() => nil_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => let
          val ys = toks_copy(xs1)
        in ys
        end
      | _ => auxmain(xs1, i+1)
in
  auxmain(xs, 0)
end


(* ****** ****** *)


fn
drop_head_tup
(xs: tok_vt): (token, tok_vt) =
  case+ xs of
  | ~nil_vt() => (TOKnil(), nil_vt())
  | ~cons_vt(x1, xs1) => (x1, xs1)


(* ****** ****** *)


fn
drop_exn_free
(xs: tok_vt, n: int): tok_vt = let
  fun
  auxmain (xs: tok_vt, i: int): tok_vt = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => let
          val () = free_token(x1)
        in xs1
        end
      | _ => let
          val () = free_token(x1)
        in auxmain(xs1, i+1)
        end
in
  auxmain(xs, 0)
end
    (* 
    without freeing the token 'x1' typechecking fails citing:

    ./tok_vt.dats: 11526(line=625, offs=9) -- 11603(line=628, offs=12):
    error(3):
    the dynamic variable [x1$419(-1)] 
    is consumed but it should be retained with the type [S2Ecst(token)] instead.

    ./tok_vt.dats: 11483(line=623, offs=7) -- 11708(line=632, offs=28):
    error(3):
    the linear dynamic variable [x1$419(-1)] 
    needs to be consumed but it is preserved with the type [S2Ecst(token)] instead.
    *)  


(* ****** ****** *)


fn
drophead_opt
(xs: !tok_vt, n: int): Option_vt(token) = let
  fun
  auxmain (xs: !tok_vt, i: int): Option_vt(token) = 
    case+ xs of
    | nil_vt() => None_vt()
    | cons_vt(x1, xs1) => 
        ifcase
        | n - i = 0 => let
            val ys = create_token(x1)
          in Some_vt(ys)
          end
        | _ => auxmain(xs1, i+1)
in
  auxmain(xs, 0)
end


(* ****** ****** *)


fn 
head_is_pred
(xs: !tok_vt, pred: !(!token) -> bool): bool =
  case+ xs of
  | nil_vt() => false
  | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => true
      | _ => false


(* ****** ****** *)


fn 
head_is_pred_free
(xs: tok_vt, pred: !(!token) -> bool): bool =
  case+ xs of
  | ~nil_vt() => false
  | ~cons_vt(x1, xs1) => let
      val () = free_toks(xs1)
    in
      ifcase
      | pred(x1) => let
          val () = free_token(x1)
        in true
        end
      | _ => let
          val () = free_token(x1) 
        in false
        end
    end



(* ****** ****** *)


(* ****** ****** *)
                 // SHOULD BE RE-WRITTEN TO GET THE LENGTH OF THE TOKEN AS WELL

fn
peek_x_depth
(xs: !toks, ot: !(!token) -> bool, ct: !(!token) -> bool): int = let
  fun
  auxmain (xs: !toks, np: int, res: int): int = 
    case+ xs of
    | nil_vt() => res
    | cons_vt(x, xs) =>
      ifcase
        | np = 0 => res - 1 // subtract last ')' ... looking for inner depth
        | ot(x) => auxmain(xs, np+1, res+1)
        | ct(x) => auxmain(xs, np-1, res+1)
        | (*else*)_ => auxmain(xs, np, res + tok_get_len(x))
in
  auxmain(xs, 1, 0) // should start from after first paren
end    

fn
peek_paren_depth
(xs: !toks): int = peek_x_depth(xs, is_opr, is_cpr)

fn
peek_square_depth
(xs: !toks): int = peek_x_depth(xs, is_osq, is_csq)

fn
peek_paren_list
(xs: toks): (toks, toks) = let
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_opr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_cpr(x) => auxmain(xs, np-1, cons_vt(x, res))
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    


fn
peek_paren_list2
(xs: toks): (toks, toks) = let
  val _ = assertloc (head_is_pred(xs, lam i => is_opr(i)))
  val xs = skip_until_in_free(xs, lam i => is_opr(i))
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_opr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_cpr(x) => auxmain(xs, np-1, cons_vt(x, res))
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    

fn
peek_square_list_osq
(xs: toks): (toks, toks) = let
  (* val () = free_token(y) *)
  val _ = assertloc (head_is_pred(xs, lam i => is_osq(i)))
  val xs = skip_until_in_free(xs, lam i => is_osq(i))
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_opr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_cpr(x) => let
            val rest = (
              if np - 1 = 0 then
                let
                  val () = free_token(x)
                in res
                end
              else
                cons_vt(x, res)
           ) : toks
          in
            auxmain(xs, np-1, rest)
            (* auxmain(xs, np-1, cons_vt(x, res)) *)
          end
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    


fn
peek_square_list_osq
(xs: toks): (toks, toks) = let
  (* val () = free_token(y) *)
  (* val _ = assertloc (head_is_pred(xs, lam i => is_osq(i))) *)
  val xs = skip_until_in_free(xs, lam i => is_osq(i))
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_osq(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_csq(x) => let
            val rest = (
              if np - 1 = 0 then
                let
                  val () = free_token(x)
                in res
                end
              else
                cons_vt(x, res)
           ) : toks
          in
            auxmain(xs, np-1, rest)
            (* auxmain(xs, np-1, cons_vt(x, res)) *)
          end
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    

fn
peek_paren_list_opr
(xs: toks): (toks, toks) = let
  val _ = assertloc (head_is_pred(xs, lam i => is_opr(i)))
  val xs = skip_until_in_free(xs, lam i => is_opr(i))
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_opr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_cpr(x) => auxmain(xs, np-1, cons_vt(x, res))
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, cons_vt(TOKopr('\('), nil_vt())) // should start from after first paren
end    


(* ****** ****** *)

fn
peek_paren_list3
(xs: toks): (toks, toks) = let
  (* val xs = skip_until_in_free(xs, lam i => is_opr(i)) *)
  fun
  auxmain (xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(res), cons_vt(x, xs))
        | is_opr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_cpr(x) => let
            val rest = (
              if np - 1 = 0 then
                let
                  val () = free_token(x)
                in res
                end
              else
                cons_vt(x, res)
           ) : toks
          in
            auxmain(xs, np-1, rest)
            (* auxmain(xs, np-1, cons_vt(x, res)) *)
          end
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    



(* ****** ****** *)

(* end of [tok_vt.dats] *)




(* fun *)
(* filter *)
(* (xs: toks, pred: token -<cloref1> bool): List0(token) = *)
(* let *)
(*   implement *)
(*   list_vt_filter$pred<token>(x) = pred(x) *)
(*   val ys = list_filter<token>(xs) *)
(* in *)
(*   list_vt2t(ys) *)
(* end *)

(* fun *)
(* exists *)
(* (xs: List0(token), pred: token -<cloref1> bool): List0(token) = *)
(* let *)
(* implement *)
(* list_exists$pred<token>(x) = pred(x) *)
(* val ys = list_exists<token>(xs) *)
(* in *)
(*   list_vt2t(ys) *)
(* end *)



(* ****** ****** *)

(*
fn 
toks_take_until
(xs: !tok_vt): tok_vt = let
  fun 
  auxmain(xs: !tok_vt, res: tok_vt): tok_vt =
  case+ xs of
  | nil_vt() => list_vt_reverse(res)
  | cons_vt(x1, xs1) => (
    let val x2 = create_token(x1)
    in
      ifcase
      | tok_is_ide(x1) => list_vt_reverse(cons_vt(x2, res))
      | _ => auxmain(xs1, cons_vt(x2, res))
    end
  )
in
  auxmain(xs, nil_vt())
end
*)

(* ****** ****** *)

(*
fn 
toks_takeskip_until
(xs: !tok_vt): (tok_vt, tok_vt) = let
  fun 
  auxmain
  (xs: !tok_vt, res: tok_vt): (tok_vt, tok_vt) = 
    case+ xs of
    | nil_vt() => (list_vt_reverse(res), nil_vt())
    | cons_vt(x1, xs1) => let 
        val x2 = create_token(x1) 
      in
        ifcase
        | tok_is_ide(x1) => let 
            val ys = toks_copy(xs1) 
          in
            (list_vt_reverse(cons_vt(x2, res)), ys)
          end
        | _ => auxmain(xs1, cons_vt(x2, res))
      end
in
  auxmain(xs, nil_vt())
end
*)