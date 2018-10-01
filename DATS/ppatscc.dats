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
  #include "./simplify_print.dats"
  #include "./print_errkind.dats"


#endif


(* ****** ****** *) 

#include "./print_location.dats"


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


fn
get_int_token
(x: !token): int = 
  case+ x of 
  | TOKint x => let
      val _ 
        = assertloc
          (g1int2int_ssize_int(length(x)) > 0)
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
  val (c5, h4, h3, c2, h1, h0) = (
      get_toks_n(ys, 5), get_toks_n(ys, 4),
      get_toks_n(ys, 3), get_toks_n(ys, 2),
      get_toks_n(ys, 1), get_toks_n(ys, 0)
    )
  val xs = (
      get_int_token(c5), get_int_token(h4),
      get_int_token(h3), get_int_token(c2),
      get_int_token(h1), get_int_token(h0)
    )
  val () = (
    free_token(c5); free_token(h4); free_token(h3); 
    free_token(c2); free_token(h1); free_token(h0)
  )
in
  xs
end  


fn
get_lineno_lengths
(ys: !toks): errline = let
  val (c5, h4, h3, c2, h1, h0) = (
      get_toks_n(ys, 5), get_toks_n(ys, 4),
      get_toks_n(ys, 3), get_toks_n(ys, 2),
      get_toks_n(ys, 1), get_toks_n(ys, 0)
    )

  val xs = (
      tok_get_len_free(c5), tok_get_len_free(h4),
      tok_get_len_free(h3), tok_get_len_free(c2),
      tok_get_len_free(h1), tok_get_len_free(h0)
    )
in
  xs
end  


fn 
print_lineno_tup
(ys: errline): void = let
  val h1 = ys.1
  val h2 = ys.2
  val h4 = ys.4
  val h5 = ys.5
  val chk = (h1 = h4) 
  val check2 = (h2 = h5)
in
  (
    print h1; print(':');
    print h2; 
    (
      if not(check2) then
      (
        print('-');
        (if not(chk) then (print h4; print(':')));
        print h5; print "\n";
      )
      else nl;
    )
  )  
end


fn
print_toktup_first_two
(x0: toks, x1: toks, ys: !toks, color: bool, lineno: bool, loc: bool): void = let
  val tups = get_lineno(ys)
  val tups_lens = get_lineno_lengths(ys)
in
  ifcase 
  | (isneqz x0 && isneqz x1) => (
      (if color then prc(magenta));
      (* print_toks_free_nonewln(x0);  *)
      print_toks(x0); // for the below to print location
      print ": "; 
      (if color then prcc);
      (
        if lineno then (print_lineno_tup(tups); (* nl; *) free_toks(x1))
        else print_toks_free(x1) 
      );
      ( // newly added - for printing loc
        if loc then (
          if (tups.1 = tups.4) && (tups.2 != tups.5)
          then (
            nl;
            get_path(x0, (tups.1, tups.3, tups.2, tups.5), color); 
            free_toks(x0);
            print_arrow(tups.2, tups.5, tups_lens.1, color); 
          ) else free_toks(x0)
        )
        else free_toks(x0)
      ) // end newly added

    )
  | _ => (free_toks(x1); free_toks(x0))
end



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
    | nil_vt() => res
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
( y: !toks, ys: toks, x0: toks, x1: toks, 
  x2: !errkind, color: bool, lineno: bool,
  loc: bool
): void =
(
  ifcase
  | toks_eq_toks(y, ys) || 
    (get_errkind_string(x2) = "last") => 
    (
      free_toks(x0); free_toks(x1); free_toks(ys)
    )
  | _ => (
      free_toks(ys); 
      nl; 
      print_toktup_first_two(x0, x1, y, color, lineno, loc); 
    )
)  
  

(* ****** ****** *)

fn
print_classified_free
(xs: errtups, color: bool, lineno: bool, loc: bool): void = let
  fun
  aux (xs: errtups, ys: toks, i: int): void =
    case+ xs of
    | ~cons_vt(x, xs) => let
        val (x0, x1, x2) = (x.0, x.1, x.2)
        val y = get_loc(x1)
        val name = get_errkind_string(x2)

        val y_ys = (if toks_eq_toks(y, ys) then i else i+1): int
        (* val () = println!() *)
        (* val () = println!(" i = ", y_ys) *)
      in
          (
            if y_ys >= 5 then 
            (
              free_toks(x0); free_toks(x1); free_errkind(x2); free_toks(y); 
              free_errtups(xs); free_toks(ys); nl
            )
            else (
              show_first2
              (y, ys, x0, x1, x2, color, lineno, loc);
              (* show_errkind(x2);  *)// optional to show errorkind string
              (if name != "ERRlast" then nl);
              print_errkind_single(x2, color); 
              nl;
              aux(xs, y, y_ys)
            )
          )
      end
    | ~nil_vt() => (free_toks(ys); nl)

in
  aux(xs, nil_vt(), 0)
end


fn
print_classified_free_n
(n: int, xs: errtups, color: bool, lineno: bool, loc: bool): void = let
  fun
  aux (xs: errtups, ys: toks, i: int): void =
    case+ xs of
    | ~cons_vt(x, xs) => let
        val (x0, x1, x2) = (x.0, x.1, x.2)
        val y = get_loc(x1)
        val name = get_errkind_string(x2)
        val y_ys = (if toks_eq_toks(y, ys) then i else i+1): int
      in
          (
            if y_ys >= n+1 then 
            (
              free_toks(x0); free_toks(x1); free_errkind(x2); 
              free_toks(y); free_errtups(xs); free_toks(ys)
            )
            else (
              show_first2
              (y, ys, x0, x1, x2, color, lineno, loc);
              (* show_errkind(x2);  *)// optional to show errorkind string
              (if name != "ERRlast" then nl);
              print_errkind_single(x2, color); 
              nl;
              aux(xs, y, y_ys)
            )
          )
      end
    | ~nil_vt() => (free_toks(ys); nl)

in
  aux(xs, nil_vt(), 0)
end


fn
print_classified_free
((* i: int,  *)xs: errtups, color: bool, lineno: bool, loc: bool): void = let

  fun
  aux (xs: errtups, ys: toks, i: int): void =
    case+ xs of
    | ~cons_vt(x, xs) => let
        val (x0, x1, x2) = (x.0, x.1, x.2)
        val y = get_loc(x1)
        val name = get_errkind_string(x2)
        val y_ys = (if toks_eq_toks(y, ys) || name = "ERRshow" then i else i+1): int
      in
          (
            if y_ys >= 11 then 
            (
              free_toks(x0); free_toks(x1); free_errkind(x2); 
              free_toks(y); free_errtups(xs); free_toks(ys);
              nl
            )
            else (
              show_first2
              (y, ys, x0, x1, x2, color, lineno, loc);
              (* show_errkind(x2);  *)// optional to show errorkind string
              (if name != "ERRlast" then nl);
              print_errkind_single(x2, color); 
              nl;
              aux(xs, y, y_ys)
            )
          )
      end
    | ~nil_vt() => (free_toks(ys); nl)

in
  aux(xs, nil_vt(), 0)
end


(* ****** ****** *)

(* end of [ppatscc.dats] *)
