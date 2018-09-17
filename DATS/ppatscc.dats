(* ****** ****** *)

#ifndef TOKENS_NONE

  #define TOKENS_NONE

  staload UN = "prelude/SATS/unsafe.sats"

  #include "./token.dats"
  #include "./tokenize.dats"
  #include "./mylib/bashstr.dats"
  #include "./token_lib.dats"
  #include "./tok_vt.dats"
  #include "./errkind.dats"
  #include "./classify_toks.dats"
  #include "./print_util.dats"
  (* #include "./print_location.dats" *)
  #include "./simplify_print.dats"
  #include "./print_errkind.dats"


#endif


(* ****** ****** *) 


fn
get_toks_n(xs: !toks, n: int): token = let
  fun 
  aux(xs: !toks, i: int): token =
    case+ xs of
    | nil_vt() => TOKnil()
    | cons_vt(x, xs) => (
        ifcase
        | n - i <= 0 => let
            val y = create_token(x)
          in
            y
          end
        | _ => aux(xs, i+1)
      )
in
  aux(xs, 0)
end

(*
(* typedef errline = (int, int, int, int, int, int) *)
typedef errline = @{ 0=int, 1=int, 2=int, 3=int, 4=int, 5=int }

fn
get_ints_errline
(xs: !toks): errline = let
  fun 
  aux(xs: !toks, i: int, res: errline): errline =
    case+ xs of
    | nil_vt() => res
    | cons_vt(x, xs) => 
        case+ x of 
        | TOKint y => let
            val _ = assertloc(g1int2int_ssize_int(length(y)) > 0)
            val x1 = $UN.strnptr2string(y)
            val r = g0string2int(x1)
          in
            aux(xs, res)
          end
        | _ => aux(xs, res)
in
  aux(xs, 0, @{ 0=0, 1=1, 2=2, 3=3, 4=4, 5=5 })
end
*)


(*
fn 
set_lineno
(ys: !toks): void = let
  // this should not be hard coded.
  val c0 = get_toks_n(ys, 5)
  val h1 = get_toks_n(ys, 4)
  val h2 = get_toks_n(ys, 3)

  val c1 = get_toks_n(ys, 2)
  val h4 = get_toks_n(ys, 1)
  val h5 = get_toks_n(ys, 0)
  val chk = tok_int_eq_tok_int(h1, h4)
in
  (
    print_token0_free(h1);
    print(':');
    print_token0_free(h2);
    print('-');
    (
      if not(chk)
      then (print_token0_free(h4); print(':'))
      else free_token(h4);
    );
    print_token0_free(h5);
  )  
end
*)


fn
get_int_token
(x: !token): int = 
  case+ x of 
    | TOKint x => let
        val _ = assertloc(g1int2int_ssize_int(length(x)) > 0)
        val x1 = $UN.strnptr2string(x)
        val res = g0string2int(x1)
      in
        res
      end
    | _ => ~1

fn
get_toks_n_ints
(xs: !toks): List0_vt(int) = let
  fun 
  aux(xs: !toks, res: List0_vt(int)): List0_vt(int) =
    case+ xs of
    | nil_vt() => res
    | cons_vt(x, xs) => aux(xs, cons_vt(get_int_token(x), res))
in
  aux(xs, nil_vt())
end

typedef errline = (int, int, int, int, int, int)

fn
get_lineno
(ys: !toks): errline = let
  val c5 = get_toks_n(ys, 5)
  val h4 = get_toks_n(ys, 4)
  val h3 = get_toks_n(ys, 3)

  val c2 = get_toks_n(ys, 2)
  val h1 = get_toks_n(ys, 1)
  val h0 = get_toks_n(ys, 0)

  val c50 = get_int_token(c5)
  val h40 = get_int_token(h4)
  val h30 = get_int_token(h3)

  val c20 = get_int_token(c2)
  val h10 = get_int_token(h1)
  val h00 = get_int_token(h0)

  val () = (
    free_token(c5); free_token(h4); free_token(h3); 
    free_token(c2); free_token(h1); free_token(h0)
  )
in
  (c50, h40, h30, c20, h10, h00)
end  

fn 
print_lineno_tup
(ys: errline): void = let
  val h1 = ys.1
  val h2 = ys.2 //- 1
  val h4 = ys.4
  val h5 = ys.5 //- 1
  val chk = (h1 = h4)
in
  (
    print h1;
    print(':');
    print h2;
    print('-');
    (
      if not(chk)
      then (print h4; print(':'))
    );
    (* (print h4;print(':')); *)
    print h5;
    print "\n";
  )  
end


fn 
print_lineno
(ys: !toks): void = let
  // this should not be hard coded.
  val h1 = get_toks_n(ys, 4)
  (* val nothing = get_int_token(h1) *)
  (* val () = println!(nothing) *)
  val h2 = get_toks_n(ys, 3)
  val h4 = get_toks_n(ys, 1)
  val h5 = get_toks_n(ys, 0)
  val chk = tok_int_eq_tok_int(h1, h4)
in
  (
    print_token0_free(h1);
    print(':');
    print_token0_free(h2);
    print('-');
    (
      if not(chk)
      then (print_token0_free(h4); print(':'))
      else free_token(h4);
    );
    print_token0_free(h5);
  )  
end


fn
print_toktup_first_two
(x0: toks, x1: toks, ys: !toks, color: bool, lineno: bool): void = let
  val tups = get_lineno(ys)
in
  ifcase 
  | (isneqz x0 && isneqz x1) => (
      (* print_toks_color_err_dir(x0); *)
      (if color then prc(magenta));
      print_toks_free_nonewln(x0); 
      print ": "; 
      (if color then prcc);
      (* print_toks(ys);  *)
      (
        (* (if color then prc(yellow)); *)
        (
        if lineno then (print_lineno_tup(tups); (* nl; *) free_toks(x1))
        else print_toks_free(x1) 
        );
        (* (if color then prcc); *)
        (* nl; *)
      );
    )
  | _ => (free_toks(x1); free_toks(x0))
end

fn
print_toktup_first_two_orig
(x0: toks, x1: toks, ys: !toks, color: bool, lineno: bool): void = 
  ifcase 
  | (isneqz x0 && isneqz x1) => (
      (if color then prc(magenta));
      print_toks_free_nonewln(x0); 
      print ": "; 
      (if color then prcc);
      (* print_toks(ys);  *)
      (
        (* (if color then prc(yellow)); *)
        (
        if lineno then (print_lineno(ys); (* nl; *) free_toks(x1))
        else print_toks_free(x1) 
        );
        (* (if color then prcc); *)
        (* nl; *)
      );
      (* free_toks(x1); (\* print_toks_free(x1) *\) *)
  )
| _ => (free_toks(x1); free_toks(x0))


(* ****** ****** *)

fn
show_loc(xs: !toks): void = let
  fun
  aux(xs: !toks): void =
    case+ xs of
    | nil_vt() => ()
    | cons_vt(x, xs) => (
      (if is_int(x) then (print_token0(x); print_space));
      aux(xs)
    )
in
  aux(xs)
end


fn
get_loc(xs: !toks): toks = let
  fun 
  aux(xs: !toks, res: toks): toks =
    case+ xs of
    | nil_vt() => res  // list_vt_reverse(res)
    | cons_vt(x, xs) => (
        ifcase
        | is_int(x) => let
            val y = create_token(x)
          in
            aux(xs, cons_vt(y, res))
          end
        | _ => aux(xs, res)
      )
in
  aux(xs, nil_vt())
end


(* ****** ****** *)


fn 
show_first2
(y: !toks, ys: toks, x0: toks, x1: toks, x2: !errkind, color: bool, lineno: bool): void =
(
  ifcase
  | toks_eq_toks(y, ys) || (get_errkind_string(x2) = "last") => (
      free_toks(x0); free_toks(x1); free_toks(ys)
    )
  | _ => (
      free_toks(ys); 
      nl; 
      print_toktup_first_two(x0, x1, y, color, lineno); 
    )
)  
  

(* ****** ****** *)


fn
print_classified_free
(xs: errtups, color: bool, lineno: bool): void = let
  fun
  aux (xs: errtups, ys: toks): void =
  case+ xs of
  | ~cons_vt(x, xs) => 
    (
      let
        val (x0, x1, x2) = (x.0, x.1, x.2)
        val y = get_loc(x1)
        val name = get_errkind_string(x2)
      in
        (
          show_first2(y, ys, x0, x1, x2, color, lineno);
          (* show_errkind(x2); *) // optional to show errorkind string
          (if name != "ERRlast" then nl);
          print_errkind_single(x2, color); 
          (* (if name != "ERRlast" then nl); *)
          nl;
          aux(xs, y)
        )
      end
    )
  | ~nil_vt() => (free_toks(ys); nl)

in
  aux(xs, nil_vt())
end


(* ****** ****** *)

(* end of [ppatscc.dats] *)