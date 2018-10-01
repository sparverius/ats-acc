#ifndef TOKENS_NONE

  #include "./tokenize.dats"
  #include "./mylib/bashstr.dats"
  #include "./token_lib.dats"
  #include "./tok_vt.dats"
  #include "./errkind.dats"

#endif


(* ****** ****** *)

staload "./../SATS/classify_toks.sats"


(* ****** ****** *)


implement
tokens_to_messages_free(xs) = let
  fun
  loop(ss: toks, res: List0_vt(toktup)): List0_vt(toktup) =
    case+ ss of
    | cons_vt(x0, xs0) => let
        // If the next message is '**SHOWTYPE[UP]**' the first char is a '*'
        //   so skip skip **SHOWTYPE[UP]** until first '(' 
        // then, (either way) get file location '/home/user/.../.../file.[d|s]ats'

        val (ys0, zs) = (
         if head_is_pred(ss, lam i => tok_chr_eq(i,'*'))
            then let
              val tmptoks = skip_until_in_free(ss, lam x => is_opr(x))
            in
              takeskip_until_free(tmptoks, lam x => is_col(x))
            end
          else takeskip_until_free(ss, lam x => is_col(x))
        ): @(toks, toks)

        val zs0 = skip_while_free(zs, lam x => is_col(x) || is_spc(x))

        val (ys1, zs1) = takeskip_until_free(zs0, lam x => is_col(x))

        val zs2 = skip_while_free(zs1, lam x => is_col(x) || is_spc(x))

        // ys0 --> path ... /home/.../../
        //  zs --> rest (': ...')
        // zs0 --> rest ('.....')
        // ys1 --> #(line=#, offs=#) -- #(line=#, offs=#)
        // zs1 --> ': error(#): ...'
        // zs2 --> 'error(#): .....'

        val (rest, zs3) = takeskip_until_free(zs2, lam x => tok_chr_eq_int(x, 10))
        // rest == cuts off end of expression
        // rest is somewhat complete
        // rest ~> (e.x. error(3): unsolved constraint: C3NSTRprop(C3TKmain(); ... ))
        // z3 is remaining or start of next expression

        // get next
        val (rest_tail, next) 
          = takeskip_until_free
            (zs3, lam x => tok_chr_eq(x, '/') || tok_ide_eq(x, "patsopt") || 
                           tok_chr_eq(x, '*') || tok_ide_eq(x, "exit"))

        val rest = list_vt_append(rest, rest_tail)
      in
        loop(next, list_vt_cons((ys0, ys1, rest), res))
      end
    | nil_vt() => let
        val () = free_toks(ss)
      in
        list_vt_reverse(res)
      end
in
  loop(xs, nil_vt())
end


(* ****** ****** *) 

fn
free_token2(x: token, y: token): void = 
(free_token(x); free_token(y))
fn
free_token3(x: token, y: token, z: token): void = 
(free_token(x); free_token(y); free_token(z))


(*
fn
check_m_toks
(xs: (token, token, token), ys: (string, string, string)): bool = 
  tok_ide_eq(xs.0, ys.0) &&
  tok_ide_eq(xs.1, ys.1) &&
  tok_ide_eq(xs.2, ys.1)
*)

(* ****** ****** *)

// BEGIN CLASSIFY

implement
parse_two(xs) = let
  val (x0, x1, x2) = (xs.0, xs.1, xs.2)
  val h = get_token_n_ide(x2, 0)
  val e = get_token_n_ide(x2, 1)
  val y = get_token_n_ide(x2, 2)
  // val () = println!(h, " ", e, " ", y)
  val hey = get_token_n_ide(x2, 4)
  val res = 
    (
      ifcase
      | tok_ide_eq(h, "the") &&
        tok_ide_eq(e, "static") &&
        tok_ide_eq(y, "expression") => @(x0, x1, ERRsimpre(x2))
      | _ => @(x0, x1, ERRexit2(x2))
    ): errtup
    val () = free_token3(h,e,y)
    val () = free_token(hey)  
in
  res
end

implement
parse_three(xs) = let
  val (x0, x1, x2) = (xs.0, xs.1, xs.2)
  val h = get_token_n_ide(x2, 0)
  val e = get_token_n_ide(x2, 1)
  val y = get_token_n_ide(x2, 2)
  // val () = println!(h, " ", e, " ", y)
  val hey = get_token_n_ide(x2, 4)
  val res = 
    (
      ifcase
      | tok_ide_eq(h, "mismatch") => (
          ifcase
          | tok_ide_eq(y, "sorts") => (
              ifcase
                | tok_ide_eq
                  (hey, "unification")     => @(x0, x1, ERRsortu(x2))
                | (*else*)_                => @(x0, x1, ERRsorts(x2))
            )
          | (* tok_ide_eq(y, "static") *)_ => @(x0, x1, ERRtyleq(x2))
        )
      | tok_ide_eq(h, "the") => (
          ifcase
          | tok_ide_eq(e, "dynamic") => (
              ifcase
              | tok_ide_eq(y, "variable")  => @(x0, x1, ERRdynvar(x2))
              | (*else*)_                  => @(x0, x1, ERRdynexp(x2))
            ) 
          | tok_ide_eq(e, "symbol")        => @(x0, x1, ERRsymbol(x2))
          | tok_ide_eq(e, "constructor")   => @(x0, x1, ERRcstpat(x2))
          (* | tok_ide_eq(e, "linear")   => @(x0, x1, ERRcstpat(x2)) *)
          | (*else*)_                      => @(x0, x1, ERRdynexp(x2))
        )
      | tok_ide_eq(h, "unsolved")          => @(x0, x1, ERRunsolv(x2))
                // nonexhaustive
      | tok_ide_eq(h, "pattern")           => @(x0, x1, ERRnonex(x2)) 
      
      | tok_ide_eq(e, "cannot")            => @(x0, x1, ERRfound(x2))
      | _                                  => @(x0, x1, ERRother(x2))
    ): errtup
    val () = free_token3(h,e,y)
    val () = free_token(hey)  
in
  res
end


(* ****** ****** *)


implement
when_err(xs) = let
  val (x0, x1, x2) = (xs.0, xs.1, xs.2)
  val y0 = drophead_opt(x2, 2)
  val-~Some_vt(y0) = y0
  val res = (
    ifcase
    | tok_ide_eq(y0, "parsing") => @(x0, x1, ERRparse(x2))
    | tok_ide_eq(y0, "lexing")  => @(x0, x1, ERRlexing(x2))
    | tok_int_eq(y0, "2")       => parse_two((x0,x1,x2))//@(x0, x1, ERRexit2(x2))
    | tok_int_eq(y0, "3")       => parse_three((x0,x1,x2))
    | (*else*)_                 => @(x0, x1, ERRother(x2)) 
  ): errtup
  val () = free_token(y0)
in
  res
end


(* ****** ****** *)


implement
classify_toks_free_n(xs, n) = let
  val () = assertloc ( n > 0 )
  fun
  aux(xs: tokstup, res: errtups, r: int): errtups =
    ifcase
    | n - r = 0 => let
        val () = free_tokstup(xs) 
      in 
        list_vt_reverse(res) 
      end
    | _ => (
      case+ xs of
      | ~nil_vt() => list_vt_reverse(res)
      | ~cons_vt(x, xs) => ( let
          val x2 = x.2
          val x0 = x.0
          val x1 = x.1
        in
        ifcase
        | isneqz x.2 => (
            ifcase
            | head_is_pred(x2, lam i => is_war(i)) => 
                aux(xs, cons_vt(@(x0, x1, ERRwarn(x2)), res), r + 1)
            | head_is_pred(x2, lam i => is_err(i)) => 
                aux(xs, cons_vt(when_err((x0,x1,x2)), res), r + 1)
            | head_is_pred(x2, lam i => tok_is_s2e(i)) => 
                aux(xs, cons_vt(@(x0, x1, ERRshow(x2)), res), r + 1)
            | iseqz xs && 
              not(head_is_pred(x2, lam i => tok_ide_eq(i, "exit"))) => 
                aux(xs, cons_vt(@(nil_vt(), nil_vt(), ERRlast((x0, x1, x2))), res), r + 1)
            | _ => aux(xs, cons_vt(@(x0, x1, ERRother(x2)), res), r + 1) 
          )
        | _ => let 
            val () = free_toks(x0)
            val () = free_toks(x1)
            val () = free_toks(x2)
            val () = free_tokstup(xs)
          in list_vt_reverse(res) 
          end
        end
      )
    )
in
  aux(xs, nil_vt(), 0)
end


(* ****** ****** *)


implement
classify_toks_free(xs) = let
  fun
  aux(xs: tokstup, res: errtups): errtups =
    case+ xs of
    | ~nil_vt() => list_vt_reverse(res)
    | ~cons_vt(x, xs) => let
        val x0 = x.0
        val x1 = x.1
        val x2 = x.2
      in
        ifcase
        | isneqz x2 => (
            ifcase
            | head_is_pred(x2, lam i => is_err(i)) => 
                aux(xs, cons_vt(when_err((x0,x1,x2)), res))
            | head_is_pred(x2, lam i => tok_is_s2e(i)) => 
                aux(xs, cons_vt(@(x0, x1, ERRshow(x2)), res))
            | head_is_pred(x2, lam i => is_war(i)) => 
                aux(xs, cons_vt(@(x0, x1, ERRwarn(x2)), res))
            (* // to introduce macro
            | head_is_pred(x2, lam i => tok_ide_eq(i, "mac")) =>
                aux(xs, cons_vt(ERRmacr((x0,x1,x2)), res))
            *)
            | iseqz xs => let 
                val () = assertloc(isneqz x0)
              in
                ifcase
                | head_is_pred(x0, lam i => tok_chr_eq(i, '/')) =>
                    aux(xs, cons_vt(@(x0, x1, ERRother(x2)), res)) 
                | _ => 
                  aux
                  (
                    xs, 
                    cons_vt
                    (@(nil_vt(), nil_vt(), ERRlast(@(x0, x1, x2))), res)
                  )
              end
            | _ => aux(xs, cons_vt(@(x0, x1, ERRother(x2)), res)) 
          )
        | _ => let 
            val () = (
              free_toks(x0); free_toks(x1); free_toks(x2); free_tokstup(xs)
            )
          in list_vt_reverse(res) 
          end
      end
in
  aux(xs, nil_vt())
end

// END OF CLASSIFY

(* ****** ****** *) 

(* end of [classify_toks.dats] *)