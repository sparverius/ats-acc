
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


(* typedef loc0 = (int, int, int, int) *)
staload "./../SATS/print_location.sats"

fn
print_location
(xs: !List0_vt(char)): void 
  = let 
    implement(env)
    list_vt_foreach$fwork<char><env>(c,env) 
      = fprint(stdout_ref, c)
in
  list_vt_foreach(xs)
end


fn
print_location_colored
(xs: !List0_vt(char), loc: loc0): void = let
  val aux_color = light_gray
  fun
  aux(xs: !List0_vt(char), i: int): void = 
    case+ xs of 
    | nil_vt() => (prcc)
    | cons_vt(x, xs) => let
        (* val () = println!(loc.1) *)
         val () = (if (i >= loc.2 - 1 && i <= loc.3 - 1) then (prc(red)) else prcc)
         val () = fprint(stdout_ref, x)
          (* val () = println!("  (i, loc.2, loc.3) = (", i, ", ", loc.2, ", ", loc.3, ")  ") *)
      in
        (aux(xs, i+1))
      end

in
  (* prc(aux_color);  *)
  aux(xs, 0);
  (* prcc;   *)
end


fn
go_to_point
(xs: !List0_vt(char), loc: loc0, color: bool): void = let
  (* val () = (assertloc(loc.0 > 0); assertloc(loc.1 >= loc.0)) *)
  fun 
  auxmain
  (xs: !List0_vt(char), n: int, res: List0_vt(char)): void 
  = (
      case+ xs of 
      | nil_vt() => let 
            val z = list_vt_reverse(res)
          in
//            print_location(z); 
            print_location_colored(z, loc);
            list_vt_free(z)
          end //list_vt_free(res)
      | cons_vt(y, ys) => (
        ifcase
        | y = '\n' => auxmain(ys, n+1, res)
        | n = loc.0 - 1 => (auxmain(ys, n, cons_vt(y, res)))
        | n > loc.0 =>  let 
            val z = list_vt_reverse(res)
          in
//            print_location(z); 
            (
              if color 
                then print_location_colored(z, loc) 
              else print_location(z)
            );
            (* prc(yellow); *)
            list_vt_free(z)
          end
        | _ => auxmain(ys, n, res)
      )
    )
in
  auxmain(xs, 0, nil_vt())
end


fn
get_point_path
(path: string, loc: loc0, color: bool): void = let
  val in_f = (fileref_open_exn(path, file_mode_r)):FILEref
  val toks = streamize_fileref_char(in_f)
  val xs = stream2list_vt(toks)
in 
(
  (* print_ident3; *)
  print(loc.0);
  print("| ");
  go_to_point(xs, loc, color);
  nl;
  fileref_close(in_f);
  list_vt_free(xs)
) 
end


fn 
tokstr_to_chrlst
(s: !Strnptr): List0_vt(char) 
= lst where {
    val x0 = strnptr_copy(s)
    val _ = assertloc(g1int2int_ssize_int(length(x0)) > 0)
    val x1 = $UN.strnptr2string(x0)
  
    val _ = assertloc(length(x1) > 0)
    val str = streamize_string_char(x1)
    val lst = stream2list_vt(str)
    val () = strnptr_free(x0)
}


fn
string_make_list_vt0
(cs: List0(char)): Strnptr1 = let
  val cs = $UN.cast{List0(charNZ)}(cs)
  val str = $effmask_wrt(string_make_list(cs))
in
  str
end



// for printing exact error location 
// i.e.
// val () = ( a :=: b )
//            ^~~~~~~
fn
print_arrow
(n: int, z: int, offset: int, color: bool): void = let
  (* val () = println!("offset = ", offset) *)
  fun
  aux(i: int): void = 
    ifcase
    (* | i = (n + offset + 2) => (print('^'); aux(i+1)) *)
    | i >= n + offset + 1 && i <= z + offset + 1 => (print('^'); aux(i+1))
    | i > z + offset + 1 => ()
    | _ => (print ' '; aux(i+1))
in
  (* println!("(n, z, offset) = (", n, ", ", z, ", ", offset, ")\n"); *)
  (if color then prc(red));
  aux(0);
  (if color then prcc)
end


fn 
get_path
(path: !List_vt(token), loc: loc0, color: bool): void = let 
  val () = assertloc(isneqz path)
  // should check path exists
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
            (* print!(str2); // uncomment to see the string rep of path *)
            get_point_path(str2, loc, color);
            list_vt_free(res); strnptr_free(str);
            (* print_arrow(loc.2, loc.3, color) *)
            (* print_arrow(loc.) *)
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


(* ****** ****** *)




(* ****** ****** *)



fn
print_path(xs: !toks): void = {
  val xs1 = toks_copy(xs)
  val ys = list_vt_reverse(xs1)
  val ys1 = take_until_free2(ys, lam x => tok_chr_eq(x, '/'))
  val ys2 = list_vt_reverse(ys1)
  val () = print_toks_free_nonewln(ys2)
}


fn
print_first_two1
(x0: !toks, x1: !toks, t: bool): void =
  if not(t) then (print_toks(x0); print ": "; print_toks(x1)) else ()

(*
fn
print_toktup_first_two
(x0: toks, x1: toks, print_ide: bool, t: bool): void = 
if (isneqz x0 && isneqz x1) then
let
  val () = (if not(t) then print "\n")
  val () = print_first_two1(x0, x1, t)
in

  (
  ifcase
  | print_ide && not(t) => let
      val location = get_location_intup_free(x1)
      (* val _ = $showtype(x0) *)
      (* val h = toks_head(x0) *)
      val y = (
        case+ x0 of
        | nil_vt() => TOKnil()
        | cons_vt(x1, xs1) => create_token(x1)
      ) : token
      (* val () = print_token0(h) *)
    in
        (
          if tok_chr_eq(y, '/')
          then ( free_token(y) ; get_path(x0, location)) //path
          else free_token(y)
        );
        free_toks(x0)
    end
  | _ =>
    (
      free_toks(x1);
      free_toks(x0)
    )
  );
  (* print "\n\n" *)
end
else (free_toks(x1); free_toks(x0))
*)


(*

fn
get_location(xs: !tok_vt): void = 

  ifcase
  | head_is_pred(xs, lam x => is_int(x)) => let
    val (first, ys) = takeskip_until(xs, lam x => is_opr(x))
    val tmp = skip_until_free(ys, lam x => tok_chr_eq(x, '-'))
    val tmp2 = skip_until_free(tmp, lam x => is_int(x))
    val snd = take_while_free(tmp2, lam x => is_int(x))
    val () = 
    (
      print_toks_free(first); print " . ";  print_toks_free(snd); 
    )
  in
  end
| _ => print_toks(xs)


fn
get_location2(xs: !tok_vt): void = 

ifcase
| head_is_pred(xs, lam x => is_int(x)) => let

  val (first, ys) = takeskip_until(xs, lam x => is_opr(x))
  val rest = skip_until_in_free(ys, lam x => tok_chr_eq(x, '='))
  val (lineno, rest2) = takeskip_until_free(rest, lam x => not(is_int(x)))
  val tmp = skip_until_free(rest2, lam x => tok_chr_eq(x, '-'))
  val tmp2 = skip_until_free(tmp, lam x => is_int(x))
  val snd = take_while_free(tmp2, lam x => is_int(x))

  val () = 
    (
      print_toks_free_nonewln(lineno); print "."; 
      print_toks_free_nonewln(first); print "-"; 
      print_toks_free_nonewln(snd)
    )
  val () = println!()
in
end
| _ => print_toks(xs)



fn
get_location_intup_free
(xs: tok_vt): (int, int) = (

  ifcase
  | head_is_pred(xs, lam x => is_int(x)) => let
  
      val (first, ys) = takeskip_until_free(xs, lam x => is_opr(x))
  
      val tmp = skip_until_free(ys, lam x => tok_chr_eq(x, '-'))
      val tmp2 = skip_until_free(tmp, lam x => is_int(x))
      val snd = take_while_free(tmp2, lam x => is_int(x))
  
      val hf = toks_head(first)
      val hs = toks_head(snd)
  
      val-TOKint(x0) = hf
      val-TOKint(x1) = hs
    
      val x01 = strnptr_copy(x0)
      val _ = assertloc(g1int2int_ssize_int(length(x01)) > 0)
      val x02 = $UN.strnptr2string(x01)
      val _ = assertloc(length(x02) > 0)
      
      val x11 = strnptr_copy(x1)
      val _ = assertloc(g1int2int_ssize_int(length(x11)) > 0)
      val x12 = $UN.strnptr2string(x11)
      val _ = assertloc(length(x12) > 0)
  
      val result = (g0string2int_int(x02), g0string2int_int(x12))

      val () = (
          strnptr_free(x01); strnptr_free(x11);
          free_token(hf); free_token(hs);
          free_toks(first); free_toks(snd)
        )
    in
      result
    end
  | _ => let
      val () = free_toks(xs)
    in 
      (0, 0) // error
    end
)

*)
