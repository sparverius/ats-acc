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

#endif


fn
print_actual
(color:bool): void =
(
  (
    if color then print_a_color("dim");  
  );
  print_str_color("actual:  ", color, "light_red")
) 


fn
print_error_statement
(xs: toks, color: bool): void = 
(
  print_toks_color(xs, color, "dim")
)


fn
print_needed
(color:bool): void = 
(
  (
    if color then print_a_color("dim");  
  );
  print_str_color("needed:  ", color, "light_green")
)


fn
print_error
(xs: toks, color: bool): void = 
(
  free_toks(xs);
  print_ident3;
  print_str_color_err("error: ", color);
  // print_toks_color_err(xs, color);
  print_after_error3;
)

fn 
print_wrap(xs: toks): void = let
  val spacing = 11
  fun
  auxmain(xs0: toks, i: int): void = 
    case+ xs0 of 
    | ~nil_vt() => () 
    | ~cons_vt(x, xs) => let
        val len = i + tok_get_len(x)
      in
        (
          ifcase
          | len >= 79 - spacing => (
              ifcase
              | is_spc(x) => 
                (
                  print_after; free_token(x); auxmain(xs, spacing)
                )
              | _ => 
                (print_after; print_token0_free(x); auxmain(xs, spacing))
            )
          | _ => (print_token0_free(x); auxmain(xs, len))
        )
      end
in
  auxmain(xs, spacing)
end



implement{}
print_simpre
(xs, color) = let
  val (error, rest) = takeskip_until_in_free(xs, lam i => is_col(i))
  val rest = drop_exn_free(rest, 0)
  val (error_statement, rest) = 
    takeskip_until_free(rest, lam i => is_osq(i))
in
//  prints 'error(...):'
    print_error(error, color);
//
    (
      if isneqz rest 
      then let
          val rest = drop_exn_free(rest, 0)
        in
          (
            print_error_statement(error_statement, color);
            // print_wrap(error_statement); 
            nl; 
            print_ident6; 
            simplify_print(rest, color);
            print_after_all;
          )
        end
      else (free_toks(rest); print_wrap(error_statement); print_after_all)
    )
end



implement{}
print_warn
(xs, color) = let
  val (error, rest) = takeskip_until_in_free(xs, lam i => is_col(i))
in
  print_error(error, color);
  print_toks_free_nonewln(rest);
  print_after_all
end


implement{}
print_parse
(xs, color) = let
  val (error, rest) = takeskip_until_in_free(xs, lam i => is_col(i))
  val rest = drop_exn_free(rest, 0)
in
(
  print_error(error, color);
  print "[parsing] "; 
  simplify_print(rest, color); 
  print_after_all
)
end

implement{}
print_other
(xs, color) = let
    val xs = take_until_free2(xs, lam i => tok_ide_eq(i, "typechecking"))
  in
    (print_ident3; print_toks_free_nonewln(xs); print_after_all)
  end


implement{}
print_lexing
(xs, color) = let
  val (error, rest) = takeskip_until_in_free(xs, lam i => is_col(i))
  val rest = drop_exn_free(rest, 0)
in
(
  print_error(error, color);
  print "[lexing] "; 
  simplify_print(rest, color); 
  print_after_all
)
end

implement{}
print_nonex
(xs, color) = let
  val (error, rest) = takeskip_until_in_free(xs, lam i => is_col(i))
  val rest = drop_exn_free(rest, 0)
  val (error_statement, pattern) = takeskip_until_free(rest, lam i => is_col(i))
  val pattern = drop_exn_free(pattern, 0)
  val pattern = take_until_free2(pattern, lam i => tok_ide_eq(i, "typechecking"))
in
(
  print_error(error, color);
  print_toks_free_nonewln(error_statement);
  print_after_message("");
  print_toks_free_nonewln(pattern);
  print_after_all
)
end


implement{}
print_found
(xs, color) = let
  val (error, t0) = takeskip_until_in_free(xs, lam i => is_col(i))
  // h0 ... error(3):
  val x0 = drop_exn_free(t0, 0)
  val (h1, t1) = takeskip_until_in_free(x0, lam i => is_col(i))

  val x0 = drop_exn_free(t1, 0)
  val (h2, t2) = takeskip_until_free(x0, lam i => is_osq(i))
  val (h3, t3) = peek_square_list_osq(t2)
  val y0 = drop_exn_free(t3, 0)
in
  (
    print_error(error, color);
    print_toks_free(h1);
    print_ident3;
    print_toks_free_nonewln(h2);
    simplify_print(h3, color);
    print_ident3_nl;
    print_toks_free_nonewln(y0);
    print_after_all
  )
end
  

implement{}
print_sorts
(xs, color): void = let
  val (error, t0) = takeskip_until_in_free(xs, lam i => is_col(i))
  val t0 = drop_exn_free(t0, 0) // drop leading space
  val (mismatch, t1) = takeskip_until_free(t0, lam i => is_col(i))
  // h1 ... ' mismatch of sorts ...'
  val t1 = drop_exn_free(t1, 1) // drop leading space
  val t2 = skip_until_free(t1, lam i => is_osq(i))
  // par ... the needed sort
  val (needed_sort, rest) = peek_square_list_osq(t2) 
  val t3 = skip_until_free(rest, lam i => is_osq(i))
  // par2 ... the actual sort
  val (actual_sort, rest2) = peek_square_list_osq(t3) 
  val () = (free_toks(rest2))
  //  val sgn = "  ~>  "
  val sgn = "  =>  "
in 
  (
    print_error(error, color);
    print_toks_free_nonewln(mismatch);
    print_after_message("");
    print_actual(color);
    simplify_print(actual_sort, color);
    print_after;
    print_needed(color);
    (* print_str_color_sgn(sgn, color); *)
    simplify_print(needed_sort, color);
    print_after_all
  ) 
end



implement{}
print_sortu
(xs, color) = let
  val (error, t0) = takeskip_until_in_free(xs, lam i => is_col(i))
  val t0 = drop_exn_free(t0, 0) // drop leading space
  val (h1, t1) = takeskip_until_free(t0, lam i => is_col(i))
  // h1 ... ' mismatch of sorts ...'
  val t1 = drop_exn_free(t1, 1) // drop leading space
  val t2 = skip_until_in_free(t1, lam i => is_col(i))
  val t2 = drop_exn_free(t2, 0) // drop leading space
  val (par, rest) = takeskip_until_free(t2, lam i => is_nwl(i))
  val t3 = skip_until_free(rest, lam i => is_col(i))
  val t3 = drop_exn_free(t3, 1)
  val (par2, rest2) = takeskip_until_free(t3, lam i => is_nwl(i))
  val () = (free_toks(rest2))
  //  val sgn = "  (=>)  "
  (* val sgn = "  ~>  " *)
  val sgn = " (should be) "
in
(
  print_error(error, color);
  print_toks_free_nonewln(h1);

  print_after_message("");

  print_actual(color);
  simplify_print(par2, color); // given

  print_after;

  print_needed(color);
  simplify_print(par, color); // needed

  print_after_all
) 
end


implement{}
print_tyleq
(xs, color) = let
  val (error, t0) = takeskip_until_in_free(xs, lam i => is_col(i))
  val (h1, t1) = takeskip_until_in_free(t0, lam i => is_col(i))
  val (h2, t2) = takeskip_until_in_free(t1, lam i => is_col(i))
  val (h3, t3) = takeskip_until_free(t2, lam i => tok_ide_eq(i, "The"))
  val (h4, t4) = takeskip_until_in_free(t3, lam i => is_col(i))

  val (* h10 *) message = drop_exn_free(h1, 0)
  val h30 = drop_exn_free(h3, 0)
  val t40 = drop_exn_free(t4, 0)
  (*
  the reason to divide by three is an estimation of
  how many fewer charachters should be left after simplification
  so / 3 (+- a few characters should work most of the time)
  *)
  val lt40 = toks_get_len(t40) / 3
  val lh30 = toks_get_len(h30) / 3

  (*
  // just for debugging  
  val () = println!()
  val () = println!("lt40 = ", lt40)
  val () = println!("lh30 = ", lh30)
  *)

  // TODO: fix magic numbers
  // the 3 is for indentation
  val len = lt40 + lh30 + 11 + 3 

   (* val sgn = "  <~  " *)
   (* val sgn = "  ~>  " *)
   val sgn = "  should be  "
   //val sgn = "\n   ~> "

  (* val sgn_overflow = "~>  " *)
  val sgn_overflow = "    should be\n          "
in
(
  free_toks(h2); free_toks(h4); 

  print_error(error, color);
  print_toks_free_nonewln(message (* h10 *));

  print_after_message("") ;

  print_actual(color);
  simplify_print(h30, color); // actual

  print_after;

  print_needed(color);
  simplify_print(t40, color); // needed

  print_after_all
)
end



implement{}
print_dynvar
(xs, color) =  let
  val (error, tail) = takeskip_until_in_free(xs, lam i => is_col(i))
  val (head11, tail1) = takeskip_until_free(tail, lam i => is_osq(i))
  val head1 = skip_while_free(head11, lam i => is_spc(i))
  val (head20, tail20) = takeskip_until_in_free(tail1, lam i => is_csq(i))
  val head2 = skip_while_free(head20, lam i => is_spc(i))
  val head2 = take_until_free2(head2, lam i => tok_chr_eq(i, '$'))
  // splits [...$...(-1)] into [... requires csq
  val tail2 = skip_while_free(tail20, lam i => is_spc(i))
  val (head_tail3, tail3) = takeskip_until_free(tail2, lam i => is_osq(i))
  val (ht, tail4) = peek_square_list_osq(tail3)
  val () = free_toks(tail4)
in 
  (
    print_error(error, color);
    print_space;
    print_toks_free_nonewln(head1);
    print_toks_free_nonewln(head2);
    print "]";
    print_ident3_nl;
    print_toks_free(head_tail3);
    print_ident6_nl;
    simplify_print(ht, color);
    print_after_all
  ) 
end


implement{}
print_cstpat
(xs, color) = let
  val (error, xs0) = takeskip_until_free(xs, lam x => is_col(x))
  val xs1 = skip_until_free(xs0, lam x => is_ide(x))
  val (error_statement, xs2) = takeskip_until_free(xs1, lam x => is_osq(x))
  val (xs3, rest) = peek_square_list_osq(xs2)
in
(
  print_error(error, color);
  free_toks(rest);

  print_after_error3;

  print_toks_free_nonewln(error_statement);

  print_after_message("");    

  print "[ ";
  simplify_print(xs3, color);
  print " ]";

  print_after_all
)
end


implement{}
print_dynexp
(xs: toks, color: bool): void = let
  (* val type_message = "TYPE::=  " *)
  val type_message = "TYPE::=  "
  val type_message = "\\_ "
  val type_message = ""

  val (error, rest) = takeskip_until_in_free(xs, lam x => is_col(x))

  val (x1, xs1) 
    = takeskip_until_in_free
      (rest, lam x => tok_ide_eq(x, "type") || tok_ide_eq(x, "value"))

  val x1 = drop_exn_free(x1, 0)

  // x0 ;; error(3):
  val () = (
    print_error(error, color)
  )

  val (error_statement, t1) = takeskip_until_free(x1, lam i => is_osq(i))
  val (inner_bracket, t2) = takeskip_until_in_free(t1, lam i => is_csq(i))
  val inner_bracket = drop_exn_free(inner_bracket, 0)

  val t2 = drop_exn_free(t2, 0)
  val error_statement = list_vt_append(error_statement, t2)
  // print statement ... 'the dynamic expression cannot ....' 
  //   however only until open square bracket
  // *** newly concatenating message
  val () = print_wrap(error_statement);
  // val () = print_toks_free_nonewln(error_statement);
  (* val () = print_after_message(type_message) *)
  // 'inner_bracket' is whatever is inside square brackets [...]
  val () = (
    if isneqz inner_bracket then let
        val inner_bracket 
          = take_until_free2(inner_bracket, lam i => tok_chr_eq(i, '$'))
        // turn xs$111(-1) into just xs
      in
        (
          (*  // experimental
          print "\"";
          print_toks_free_nonewln(inner_bracket); 
          print "\"";
          *)
          free_toks(inner_bracket);
        ) 
      end
    else free_toks(inner_bracket)
  )
  // [.....] (h2)
  // is/needs ... (t2)
  // end new
in
  if isneqz xs1 && not(head_is_pred(xs1, lam i => tok_chr_eq(i, '.'))) 
  then let
      val xs2 = drop_exn_free(xs1, 0)
      val (par, rest) = peek_square_list_osq(xs2)
  in 
    (
      (
        if isneqz par then
        (
        (* nl; *) // ... cannot be assigned the type
        print_after_message(type_message); // print_ident6_nl;
        print "[";
        simplify_print(par, color); 
        print "]";
        )
        else free_toks(par)
      );
      free_toks(rest);
      // expiremental
      print_after_all      
      //
    ) 
  end
  else (
    free_toks(xs1);
    // expiremental
    print_after_all    
    //
    )
end



implement{}
print_unsolv // newest
(xs, color) = let

  val (x0, xs0) = takeskip_until_in_free(xs, lam x => is_col(x))
  //  val () 
  // = print_error
  // (list_vt_reverse_append(x0,list_vt_sing(TOKcol(':'))), color);
  val () = print_error(x0, color)
  //  val xs1 = drop_exn_free(xs0, 1)
  val xs1 = drop_exn_free(xs0, 0)
  val (x1, xs2) = takeskip_until_free(xs1, lam x => is_col(x))
  val () = print_toks_free_nonewln(x1)
  val xs3 = drop_exn_free(xs2, 1)
  val xs5  = take_until_free2(xs3, lam x => tok_ide_eq(x, "typechecking")) 
    // in case last
    (* at this point xs5 contains the tree *)
  val (hs, ts) = takeskip_until_free(xs5, lam x => tok_is_s2e(x))
  val () = free_toks(hs)
  // -could remove this ??
  val (ts, rest0) = peek_paren_list3(ts)
  val () = free_toks(rest0)
  val (h00, t00) = drop_head_tup(ts) // h00 ... 'S2**'
in
  ifcase
  | tok_s2e_eq(h00, "S2Eeqeq") => let
      val () = free_token(h00)
    val (h01, ts0) = drop_head_tup(t00) // h01 ... '\('
    val () = free_token(h01)
    val (par, rest) = peek_paren_list(ts0)
    val () = free_toks(rest)
    val (name1, partail) = takeskip_until_in_free(par, lam x => is_opr(x))
    val (name1_rest, partail2) = peek_paren_list3(partail)
    val partail_tail 
      = skip_while_free(partail2, lam i => is_spc(i) || is_sco(i))
    val first0 = list_vt_append(name1, name1_rest)
    val sgn = "  ~?>  ";
    in
      ( 
        if isneqz first0 then
        (
          let 
            val (partail_tail, toss) = peek_paren_list3(partail_tail)
            val () = free_toks(toss)
          in 
            (* nl; // ???? is this needed? *)
            (* print_ident6_nl; *)
            print_after_message("");
          //  print ("actual: "); 
          print_actual(color);
            simplify_print(first0, color); // actual
          print_after;
            // print_str_color_sgn(sgn, color);
          //  print ("needed: "); 
          print_needed(color);
            simplify_print(partail_tail, color); // needed
            print_after_all
          end
        ) else (free_toks(first0); free_toks(partail_tail); )
      );
    end 
  | tok_s2e_eq(h00, "S2Eapp") => let
      val xs = cons_vt(h00, t00)
    in
      (
        nl;
        print_ident6_nl;
        simplify_print(xs, color); 
        print_after_all
      )
    end
  | _ => let
      val xs = cons_vt(h00, t00)
    in
      print_toks_free(xs); print_after_all
    end
end


implement{}
print_exit2
(xs, color) = let
  val (error, ts) = takeskip_until_in_free(xs, lam i => is_col(i))
  val () = print_error(error, color)
  val xs = drop_exn_free(ts, 0)
  fun
  auxmain(xs0: toks, i: int): void = 
    case+ xs0 of 
    | ~nil_vt() => (print_after_all) 
    | ~cons_vt(x, xs) => let
        val len = i + tok_get_len(x)
      in
        (
          ifcase
          | len >= 79 => (
              ifcase
              | is_spc(x) => 
                (nl; print_ident3(* _nl *); free_token(x); auxmain(xs, 3))
              | _ => 
                (nl; print_ident3(* _nl *);print_token0_free(x); auxmain(xs, 3))
            )
          | _ => (print_token0_free(x); auxmain(xs, len))
        )
      end
in
  auxmain(xs, 3)
end


implement{}
print_symbol
(xs, color) = let
    val (error, t0) = takeskip_until_in_free(xs, lam i => is_col(i))
    val l1 = drop_exn_free(t0, 0)
    val (h1, t1) = takeskip_until_in_free(l1, lam i => is_nwl(i))
    val (h2, t2) = takeskip_until_in_free(t1, lam i => is_nwl(i))
    val (h3, t3) = takeskip_until_in_free(t2, lam i => is_nwl(i))
in
  (
    print_error(error, color);
    print_toks_free_nonewln(h1);
    (
      if isneqz h2 then (
        print_after_message("");
        print_toks_free_nonewln(h2);
        print_after_message("");
        print_toks_free_nonewln(h3);
        free_toks(t3)
      ) else (
        free_toks(h2); free_toks(h3); free_toks(t3)
      )
    ); 
    print_after_all
  ) 
end


implement{}
print_last
(xs, color) = let
  // val () = nl // for two lines before final message
  val (x0, x1, x2) = (xs.0, xs.1, xs.2)
  val (h,t) = takeskip_until_free(x1, lam i => tok_ide_eq(i, "exit"))
  val x3 = skip_until_free(x2, lam i => is_opr(i))
in 
  (
    (
      if isneqz x0 then (
        // print_toks_free_nonewln(x0); print ": "
        print_toks_color(x0, color, "yellow"); 
        print_str_color(": ", color, "yellow")
      ) 
      else free_toks(x0)
    );
    (* print_toks_color(h, color, "yellow");  *)
    print_toks_free_nonewln(h);
    ( 
      if head_is_pred(t, lam i => tok_ide_eq(i, "exit")) 
      then (nl; print "      ")
    );
    ( 
      (* print_toks_free_nonewln(t); print ": " *)
      print_toks_color(t, color, "yellow");
      print_str_color(": ", color, "yellow")
    );
    print_toks_free_nonewln(x3);
    (* print_after_all *)
    // print_toks_color_err(x3, color);
  ) 
end


implement{}
print_show
(xs, color) = let
  (* val type_symbol = "=T=  "  *)
  val type_symbol = "SHOWTYPE:: " 
in
(
  (* print_ident3; *)
  print_ident6;
  (* print "=T=  "; *)
  (* print_str_color_show("=T=  ", color); *)
  print_str_color_show(type_symbol, color);
  print_ident6_nl;
  simplify_print(xs, color);
  nl;
)
end


implement{}
print_unit(): void = print "ERRunit"


(* ****** ****** *)

(* end of [print_errkind.dats] *)