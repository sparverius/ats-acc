
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


staload "./../SATS/print_location.sats"


implmnt
print_location
(xs) = let
    implement(env)
    list_vt_foreach$fwork<char><env>(c,env) 
      = fprint(stdout_ref, c)
in
  list_vt_foreach(xs)
end


implmnt
print_location_colored
(xs, loc) = let
  val aux_color = light_gray
  fun
  aux(xs: !List0_vt(char), i: int): void = 
    case+ xs of 
    | nil_vt() => (prcc)
    | cons_vt(x, xs) => let
        val () = (
          if (i >= loc.2 - 1 && i <= loc.3 - 2(* 1 *)) 
          then (prc(red)) else prcc
        )
        val () = fprint(stdout_ref, x)
      in
        (aux(xs, i+1))
      end
in
  aux(xs, 0);
end


implmnt
go_to_point
(xs, loc, color) = let
  // val () = (assertloc(loc.0 > 0); assertloc(loc.1 >= loc.0))
  fun 
  auxmain
  (xs: !List0_vt(char), n: int, res: List0_vt(char)): void  = (
    case+ xs of 
    | nil_vt() => let 
        val z = list_vt_reverse(res)
      in
        (
          if color 
          then print_location_colored(z, loc) 
          else print_location(z)
        );
        list_vt_free(z)
      end
    | cons_vt(y, ys) => (
      ifcase
      | y = '\n' => auxmain(ys, n+1, res)
      | n = loc.0 - 1 => (auxmain(ys, n, cons_vt(y, res)))
      | n > loc.0 =>  let 
          val z = list_vt_reverse(res)
        in
          (
            if color 
            then print_location_colored(z, loc) 
            else print_location(z)
          );
          list_vt_free(z)
        end
      | _ => auxmain(ys, n, res)
    )
  )
in
  auxmain(xs, 0, nil_vt())
end


implmnt
get_point_path
(path, loc, color) = let
  val in_f = (fileref_open_exn(path, file_mode_r)):FILEref
  val toks = streamize_fileref_char(in_f)
  val xs = stream2list_vt(toks)
in 
(
  print(loc.0);
  print("| ");
  go_to_point(xs, loc, color);
  nl;
  fileref_close(in_f);
  list_vt_free(xs)
) 
end


implmnt
tokstr_to_chrlst
(s) = lst where {
    val x0 = strnptr_copy(s)
    val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
    val x1 = $UN.strnptr2string(x0)
  
    val _ = assertloc(length(x1) > 0)
    val str = streamize_string_char(x1)
    val lst = stream2list_vt(str)
    val () = strnptr_free(x0)
}


implmnt
string_make_list_vt0
(cs) = let
//(cs: List0(char)): Strnptr1
  val cs = $UN.cast{List0(charNZ)}(cs)
  val str = $effmask_wrt(string_make_list(cs))
in
  str
end


implmnt
print_arrow
(n, z, offset, color) = let
    // for printing exact error location 
    // i.e.
    // val () = ( a :=: b )
    //            ^~~~~~~
  val arrow = "^"
  fun
  aux(i: int): void = 
    ifcase
    | i >= n + offset + 1 && i <= z + offset (* + 1 *) => 
      (
        print_str_color_err(arrow, color);
        aux(i+1)
      )
    | i > z + offset + 1 => ((* nl;  *)if color then prcc)
    | _ => (print ' '; aux(i+1))
in
  aux(0);
end


implmnt
get_path
(path, loc, color) = let
  // should check path exists
  val () = assertloc(isneqz path) 
  fun 
  auxmain(xs: !List0_vt(token), res: List0_vt(char)): void = 
    case+ xs of 
    | nil_vt() => (
        ifcase
        | list_vt_is_cons(res) => let
            val tmp = $UN.list_vt2t(res)
            val str = string_make_list_vt0(tmp)
            val str2 = $UN.strnptr2string(str)
          in
            
            // print!(str2); // uncomment to see the string rep of path
            get_point_path(str2, loc, color);
            list_vt_free(res); strnptr_free(str);
          end
        | _ => list_vt_free(res)
      )
    | cons_vt(x, xs) => (
      case+ x of 
      | TOKide s => auxmain(xs, list_vt_append(res, tokstr_to_chrlst(s)))
      | TOKint s => auxmain(xs, list_vt_append(res, tokstr_to_chrlst(s)))
      | TOKwar s => auxmain(xs, list_vt_append(res, tokstr_to_chrlst(s)))
      | TOKerr s => auxmain(xs, list_vt_append(res, tokstr_to_chrlst(s)))
      | TOKs2e s => auxmain(xs, list_vt_append(res, tokstr_to_chrlst(s)))
      // characters
      | TOKchr c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKcol c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKsco c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKopr c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKcpr c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKosq c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKcsq c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      | TOKspc c => auxmain(xs, list_vt_append(res, list_vt_sing(c)))
      //
      | TOKnil() => auxmain(xs, list_vt_reverse(cons_vt(' ', res)))
    ) 
in
  auxmain(path, nil_vt())
end


implmnt
print_path
(tokens) = {
  val xs1 = toks_copy(tokens)
  val ys = list_vt_reverse(xs1)
  val ys1 = take_until_free2(ys, lam x => tok_chr_eq(x, '/'))
  val ys2 = list_vt_reverse(ys1)
  val () = print_toks_free_nonewln(ys2)
}


(* ****** ****** *)

(* end of [print_location.dats] *)