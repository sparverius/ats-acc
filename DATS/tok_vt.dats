(* ****** ****** *)

#ifndef TOKENS_NONE

  #define TOKENS_NONE
  #include "./token.dats"
  staload UN = "prelude/SATS/unsafe.sats"
  #include "./token_lib.dats"

#endif

(* ****** ****** *)

staload "./../SATS/token.sats"
staload "./../SATS/vtypes.sats"
staload "./../SATS/tok_vt.sats"

(*
vtypedef toks = List0_vt(token)

vtypedef toktup = @(toks, toks, toks)

vtypedef tokstup = List0_vt(toktup)
*)

(* ****** ****** *)

implement
free_toks(xs)
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


implement
free_toktup(xs)
  = (
      free_toks(xs.0); free_toks(xs.1); free_toks(xs.2)
    )


implement
free_tokstup(xs) = let
  fun auxmain(xs0: tokstup): void = 
    case+ xs0 of
    | ~nil_vt() => ()
    | ~cons_vt(y, ys) => (free_toktup(y); auxmain(ys))
in
  auxmain(xs)
end


(* ****** ****** *)


implement
printall(xs) = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = fprint_token(stdout_ref, c)
in
  list_vt_foreach(xs)
end


(* ****** ****** *)


implement
print_token0(x) =
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


implement
print_token0_free(x) = 
  case+ x of
  | ~TOKide ide => (
      (
        ifcase 
        | ide = "d0exp" => print "a dynamic expression"
        | _ => print ide
      ); 
      strnptr_free(ide)
    )
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

(* ****** ****** *)

// start additions 9/10

implement
print_token0_free_nl(x) = 
  case+ x of
  | ~TOKide ide => (print ide; strnptr_free(ide))
  | ~TOKint itn => (print itn; strnptr_free(itn))
  | ~TOKs2e s2e => (print s2e; strnptr_free(s2e))
  | ~TOKwar war => (print war; strnptr_free(war))
  | ~TOKerr err => (print err; strnptr_free(err))
  | ~TOKchr chr => (print chr) 
  | ~TOKcol col => (print col) 
  | ~TOKsco sco => (print sco) 
  | ~TOKopr opr => (print opr) 
  | ~TOKcpr cpr => (print cpr) 
  | ~TOKosq osq => (print osq) 
  | ~TOKcsq csq => (print csq) 
  | ~TOKspc spc => (print spc) 
  | ~TOKnil _ => print "nil"


implement
print_toks_all(xs) = let
  fun
  auxmain (xs: toks): void = 
    case+ xs of
    | ~nil_vt() => ()
    | ~cons_vt(x, xs) => ( 
        print_token0_free_nl(x); 
        auxmain(xs)
      )
in
  auxmain(xs)
end

// end additions 9/10

(* ****** ****** *)


implement
print_toks(xs) = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = print_token0(c)
in
  list_vt_foreach(xs)
end


implement
print_toks_tokens(xs) = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = fprint_token(stdout_ref, c)
in
  list_vt_foreach(xs)
end


implement
print_toks_no_newline(xs) = let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (if not(is_nwl(c)) then print_token0(c))
in
  list_vt_foreach(xs)
end


implement
print_toks_token(xs) = let 
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (fprint_token(stdout_ref, c); fprint(stdout_ref, ", "))
in
  list_vt_foreach(xs); print "\n"
end


implement
print_toks_delim(xs, delim) = let 
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) 
    = (fprint_token(stdout_ref, c); fprint(stdout_ref, delim))
in
  list_vt_foreach(xs); print "\n"
end


implement
print_tokens_free(xs) = let
  fun
  auxmain (xs: toks): void = 
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


implement
print_toks_free_base(xs) = let
  fun
  auxmain (xs: toks): void = 
    case+ xs of
    | ~nil_vt() => ()
    | ~cons_vt(x, xs) => (print_token0_free(x); auxmain(xs))
in
  auxmain(xs)
end


implement
print_toks_free(xs) = (print_toks_free_base(xs); print "\n")


implement
print_toks_free_nonewln(xs) = (print_toks_free_base(xs))


implement
print_toks_delim_free(xs, delim) = let
  fun
  auxmain (xs: toks): void = 
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


implement
toks_get_len(tok) = let
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


implement
toks_copy(xs) = let
  fun 
  auxmain (xs: !toks, res: toks): toks = 
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


implement
toks_eq_toks(xs, ys) = let
  fun
  aux(xs: !toks, ys: !toks): bool = 
    case+ (xs, ys) of
    | (cons_vt(x, xs), cons_vt(y, ys)) =>
        if tok_int_eq_tok_int(x, y) then aux(xs, ys) else false
    | (nil_vt(), nil_vt()) => true
    | (_, _) => false
in
  aux(xs, ys)
end


(* ****** ****** *)
(*
typedef cfun(a:t@ype, b:t@ype) = a -<cloref1> b
vtypedef cfun_vt( a: vt@ype, b:t@ype) = a -<cloref1> b
*)
(* ****** ****** *)


implement
take_until_free(xs, pred) = let
  fun 
  auxmain(xs: toks, res: toks): toks =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => (
        ifcase
        | pred(x1) => let 
            val () = free_toks(xs1) 
          in list_vt_reverse(cons_vt(x1, res))
          end
        | _ => auxmain(xs1, cons_vt(x1, res))
      )

in
  auxmain(xs, nil_vt())
end


implement
take_until_free2(xs, pred) = let
  fun 
  auxmain(xs: toks, res: toks): toks =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x1, xs1) => (
        ifcase
        | pred(x1) => let 
            val () = free_toks(xs1) 
            val () = free_token(x1)
          in list_vt_reverse(res)
          end
        | _ => auxmain(xs1, cons_vt(x1, res))
      )
in
  auxmain(xs, nil_vt())
end



(* ****** ****** *)


implement
take_while_free(xs, pred)  = let
  fun 
  auxmain (xs: toks, res: toks): toks =
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


implement
takeskip_until_free
(xs, pred) = let
  fun
  loop(xs: &toks >> toks) : toks = (
    ifcase
    | isneqz xs => let
        val res = 
        (
          ifcase
          | head_is_pred(xs, pred) => let
              val res = xs
              val () = xs := nil_vt()
            in
              res
            end
          | _ => let
              val+@cons_vt(x, xs1) = xs
              val res = loop(xs1)
              prval () = fold@ (xs)
            in
              res
            end
        ): toks
      in
        res
      end
    | _ => let
        val res = xs
        val () = xs := nil_vt()
      in
        res
      end
  )
  var xs = xs
  val res = loop(xs)
in
  (xs, res)
end


(* ****** ****** *)

(*
implement
takeskip_until_free(xs, pred) = let
  fun 
  auxmain(xs: toks, res: toks): @(toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x1, xs1) =>
        ifcase
        | pred(x1) => (list_vt_reverse(res), cons_vt(x1, xs1))
        | _ => auxmain(xs1, cons_vt(x1, res))
in
  auxmain(xs, nil_vt())
end
*)

(* ****** ****** *)


implement
takeskip_until_in_free(xs, pred) = let
  fun
  auxmain(xs: toks, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) 
        => (list_vt_reverse(cons_vt(x1, res)), xs1)
      | _ 
        => auxmain(xs1, cons_vt(x1, res))
in
  auxmain(xs, nil_vt())
end


(* ****** ****** *)


implement
skip_while_free(xs, pred) = let
  fun
  auxmain (xs: toks): toks = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => let
          val () = free_token(x1)
        in 
          auxmain(xs1)
        end
      | _ => cons_vt(x1, xs1)
in
  auxmain(xs)
end


(* ****** ****** *)


implement
skip_until_free(xs, pred) = let
  fun 
  auxmain (xs: toks): toks = 
    case+ xs of
    | ~nil_vt() => nil_vt()
    | ~cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => cons_vt(x1, xs1)
      | _ => let
          val () = free_token(x1)
        in
          auxmain(xs1)
        end
in
  auxmain(xs)
end


(* ****** ****** *)


implement
skip_until_in_free(xs, pred) = let
  fun 
  auxmain (xs: toks): toks = 
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


implement
toks_head(xs) = 
  case+ xs of
  | nil_vt() => TOKnil()
  | cons_vt(x1, xs1) => create_token(x1)


(* ****** ****** *)


implement
toks_head_free(xs) =
  case+ xs of
  | ~nil_vt() => TOKnil()
  | ~cons_vt(x1, xs1) => let
      val () = free_toks(xs1)
    in x1
    end


(* ****** ****** *)


implement
toks_head_tail_free(xs) = 
  case+ xs of
  | ~nil_vt() => (TOKnil(), nil_vt())
  | ~cons_vt(x1, xs1) => (x1, xs1)


(* ****** ****** *)

(*
implement
head_is_pred(xs, pred) =
  case+ xs of
  | nil_vt() => false
  | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => true
      | _ => false
*)

(* ****** ****** *)


implement
print_toks_free_nonewln_skip1(xs) = let
  val (x, xs) = toks_head_tail_free(xs)
  val () = free_token(x)
  fun
  auxmain (xs: toks): void = 
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


implement
drop_opt(xs, n) = let
  fun
  auxmain (xs: !toks, i: int): Option_vt(toks) = 
    case+ xs of
    | nil_vt() => None_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => 
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


implement
drop_opt_free(xs, n) = let
  fun
  auxmain (xs: toks, i: int): Option_vt(toks) = 
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


implement
token_is_pred(xs, n, pred) = let
  fun
  auxmain (xs: !toks, i: int): bool = 
    case+ xs of
    | nil_vt() => false
    | cons_vt(x1, xs1) => let
      in
        ifcase
        | n - i = 0 => let val () = print_token0(x1) in pred(x1) end
        | _ => auxmain(xs1, i+1)
      end
in 
  auxmain(xs, 0)
end


(* ****** ****** *)


implement
get_token_at(xs, n) = let
  fun
  auxmain (xs: !toks, i: int): Option_vt(token) = 
    case+ xs of
    | nil_vt() => None_vt()
    | cons_vt(x1, xs1) => 
      ifcase
      | n - i = 0 => let 
          val y = create_token(x1)
        in Some_vt(y)
        end
      | _ => auxmain(xs1, i+1)
in 
  auxmain(xs, 0)
end


implement
get_token_at_unsafe(xs, n) = let
// unsafe in that it will return TOKnil if error
  fun
  auxmain (xs: !toks, i: int): token = 
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


implement
get_token_n_ide(xs, n) = let
  fun
  auxmain (xs: !toks, i: int): token = 
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


implement
drop_exn(xs, n) = let
  // ERR: does not throw exception 
  fun
  auxmain (xs: !toks, i: int): toks = 
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


implement
drop_head_tup(xs) =
  case+ xs of
  | ~nil_vt() => (TOKnil(), nil_vt())
  | ~cons_vt(x1, xs1) => (x1, xs1)


(* ****** ****** *)


implement
drop_exn_free(xs, n) = let
  fun
  auxmain (xs: toks, i: int): toks = 
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


(* ****** ****** *)


implement
drophead_opt(xs, n) = let
  fun
  auxmain (xs: !toks, i: int): Option_vt(token) = 
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


implement
toks_pred0_is_pred1(xs, pred0, pred1, n) = let
  fun
  auxmain(xs: !toks, i: int): bool = 
    case+ xs of
    | nil_vt() => false
    | cons_vt(x1, xs1) => 
      ifcase
        | n - i = 0 && pred0(x1) => pred1(x1)
      | pred0(x1) => auxmain(xs1, i+1)
      | _ => auxmain(xs1, i)
in 
  auxmain(xs, 0)
end

fn
toks_n_is_pred_ide
(xs: !toks, pred: !(!token) -> bool, n: int): bool = 
  toks_pred0_is_pred1(xs, is_ide, pred, n)


implement
head_is_pred(xs, pred) =
  case+ xs of
  | nil_vt() => false
  | cons_vt(x1, xs1) => 
      ifcase
      | pred(x1) => true
      | _ => false


(* ****** ****** *)


implement
head_is_pred_free(xs, pred) =
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


implement
peek_x_depth
(xs, ot, ct) = let
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


implement
peek_paren_depth(xs) = peek_x_depth(xs, is_opr, is_cpr)

implement
peek_square_depth(xs) = peek_x_depth(xs, is_osq, is_csq)


(* ****** ****** *)


implement
peek_paren_list(xs) = let
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


implement
peek_paren_list2(xs) = let
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


implement
peek_square_list_osq(xs) = let
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


implement
peek_paren_list_opr(xs) = let
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


implement
peek_paren_list3(xs) = let
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
          end
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, nil_vt()) // should start from after first paren
end    


fn
peek_paren_list_for_fun
(xs: toks): (toks, toks) = let
  val (head, xs) = takeskip_until_free(xs, lam i => is_opr(i))
  fun
  auxmain(head: toks, xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_append(head, list_vt_reverse(res)), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_append(head, list_vt_reverse(res)), cons_vt(x, xs))
        | is_opr(x) => auxmain(head, xs, np+1, cons_vt(x, res))
        | is_cpr(x) => let
            val rest = (
              if np - 1 = 0 then
                let
                  val () = free_token(x)
                in 
                  res
                end
              else
                cons_vt(x, res)
           ) : toks
          in
            auxmain(head, xs, np-1, rest)
          end
        | (*else*)_ => auxmain(head, xs, np, cons_vt(x, res))
in
  auxmain(head, xs, 1, nil_vt())
end    

fn
peek_paren_list_reverse
(xs: toks): (toks, toks) = let
  (* val (head, xs) = takeskip_until_free(xs, lam i => is_opr(i)) *)
  fun
  auxmain(xs: toks, np: int, res: toks): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x, xs) =>
      ifcase
        | np = 0 => (list_vt_reverse(cons_vt(x, res)), xs)
        | is_cpr(x) => auxmain(xs, np+1, cons_vt(x, res))
        | is_opr(x) => auxmain(xs, np-1, cons_vt(x, res))
        | (*else*)_ => auxmain(xs, np, cons_vt(x, res))
in
  auxmain(xs, 1, list_vt_sing(TOKcpr(')')))
end    


fn
takeskip_until_in_free_rev
(xs: toks, pred: !(!token) -> bool): (toks, toks) = let
  fun
  auxmain(xs: toks, res: toks, i: int): (toks, toks) = 
    case+ xs of
    | ~nil_vt() => (list_vt_reverse(res), nil_vt())
    | ~cons_vt(x1, xs1) => 
      ifcase
      | is_cpr(x1) => auxmain(xs1, cons_vt(x1, res), i+1)
      | is_opr(x1) => auxmain(xs1, cons_vt(x1, res), i-1)
      | pred(x1) && i = 0 
        => (list_vt_reverse(cons_vt(x1, res)), xs1)
      | _ 
        => auxmain(xs1, cons_vt(x1, res), i)
in
  auxmain(xs, nil_vt(), 0)
end


(* ****** ****** *)

(* end of [tok_vt.dats] *)
