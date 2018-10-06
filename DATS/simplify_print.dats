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

#endif


staload "./../SATS/simplify_print.sats"


(*
  Currently handles only a subset of error expressions from "ATS2/src/pats_staexp2.sats"
  - expect additions
  - output format will improve
  - need advice for general format

  some notation :: 
    
         #  :  prefix for external value or external kind (S2Eextype | S2Eextkind) 
       [n]  :  existentially qualified type n (S2Eexi)
       inf  :  intinf (S2Eintinf)
      uni.  :  universally quantified type (S2Euni)
      
   "g0int"  :  "g0int_t0ype"
 "g0float"  :  "g0float_t0ype"
  "g0uint"  :  "g0uint_t0ype"
   "g1int"  :  "g1int_int_t0ype"
    "char"  :  "char_t0ype"
    "char"  :  "char_int_t0ype"
  "string"  :  ""string_type"
  "string"  :  "string_int_type"  // even though indexed?
 "strnptr"  :  "strnptr_addr_int_vtype"
    "list"  :  "list_t0ype_int_type"
 "list_vt"  :  "list_vt0ype_int_vtype"

     XXXX   :  where X is an integer 
     XXXXX       
*)



implmnt(*{}*)
get_simplified_idestring
(idestring: string): string = 
  case+ idestring of
  | "d0exp" =>  "dynamic expression"
  | "g0int_t0ype" =>  "g0int"
  | "g0float_t0ype" =>  "g0float"
  | "g0uint_t0ype" =>  "g0uint"
  | "g1int_int_t0ype" =>  "g1int"
  | "char_t0ype" =>  "char"
  | "char_int_t0ype" =>  "char"
  | "bool_t0ype" =>  "bool"
  | "string_type" =>  "string"
  | "string_int_type" =>  "string"// even though indexed?
  | "strnptr_addr_int_vtype" =>  "strnptr"
  | "list_t0ype_int_type" =>  "list"
  | "list_vt0ype_int_vtype" =>  "list_vt"
  | "list0_t0ype_type" => "list0"
  | _ => idestring



implmnt(*{}*)
print_simplified_idestring
(idestring: string, color_name: string, color: bool): void = (
  ifcase 
  | idestring = "atstype_int" || idestring = "int_kind" => (
    (if color then (print_a_color("dim"); print_a_color("yellow")));
    print(idestring);
    (if color then prcc)
  )
  | _ => (
    if color then (
      print_a_color("reset_all");
      print_a_color(color_name);
      print idestring;
      prcc
    )
    else print idestring
  )
)


implmnt(*{}*)
just_print
(x0: token, color: bool, color_name: string): void = 
(
  (if color then print_a_color(color_name));
  print_token0_free(x0); 
  (if color then prcc);            
)


implmnt(*{}*)
simplify_whats_inside
(xs0: toks) : (toks, toks) = let
  val xs00 = drop_exn_free(xs0, 0)
  val (par, rest) = peek_paren_list3(xs00)
in
  (par, rest)
end


implmnt(*{}*)
generic_simplify
(x0: token, xs0: toks, insert: string): void = let
  val (expression, rest) = simplify_whats_inside(xs0)
in
  (
    free_token(x0); 
    free_toks(rest);
    print (insert); 
    print_toks_free_nonewln(expression); 
  )  
end


(* ****** ****** *) // print simplified


implmnt(*{}*)
simplify_idestring
(x0: token, color: bool): void = 
  case- x0 of
  | ~TOKide(i) => let
      val _ = assertloc(g1int2int_ssize_int(length(i)) > 0)
      val str = $UN.strnptr2string(i)
      val simplified_idestring = get_simplified_idestring(str)
    in
      (
      (* val () = (if color then print_a_color("cyan")) *)
      print_simplified_idestring(simplified_idestring, "cyan", false(*color*));
      (* val () = (if color then prcc) *)
      strnptr_free(i)
      )
    end


(* ****** ****** *) // print simplified S2E


(* 
  from *** ATS2/src/pats_staexp2.sats *** (line 477-557 and ~559-~582) 
  datatype s2exp_node
*)


    (*
    | S2Eint of int // integer
    *)

implmnt(*{}*)
simplify_S2Eint
(x0: token, xs0: toks): void = generic_simplify(x0, xs0, "")


    (*
    | S2Eintinf of intinf // integer of flex precision
    *)

implmnt(*{}*)
simplify_S2Eintinf
(x0: token, xs0: toks): void = generic_simplify(x0, xs0, "inf ")


    (*
    | S2Efloat of string // static floating-points
    *)

implmnt(*{}*) 
simplify_S2Efloat
(x0: token, xs0: toks): void = generic_simplify(x0, xs0, "")


    (*
    | S2Estring of string // static string constants
    *)

implmnt(*{}*) 
simplify_S2Estring
(x0: token, xs0: toks): void = generic_simplify(x0, xs0, "")


    (*
    | S2Ecst of s2cst // constant
    *)

implmnt(*{}*)
simplify_S2Ecst
(x0: token, xs0: toks): (toks, toks) = let
  val () = free_token(x0)
  val xs00 = drop_exn_free(xs0, 0)
  val (par, rest) = peek_paren_list3(xs00)
in 
  (par, rest)
end
    // helper
implmnt(*{}*)
simplify_S2Eext
(x0: token, xs0: toks): (toks) = let
  val () = free_token(x0)
  val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
  val (expression, rest) = peek_paren_list3(s0)
  val () = print "# "  // val () = print "ext. "
  val () = free_toks(rest)
in
  expression
end


    (*
    | S2Eextype of (string(*name*), s2explstlst) // external type
    *)

implmnt(*{}*)
simplify_S2Eextype
(x0: token, xs0: toks): (toks) = simplify_S2Eext(x0, xs0)


    (*
    | S2Eextkind of (string(*name*), s2explstlst) // external tkind
    *)

implmnt(*{}*)
simplify_S2Eextkind
(x0: token, xs0: toks): (toks) = simplify_S2Eext(x0, xs0)


    (*
    | S2Evar of s2var // universal variable
    *)

implmnt(*{}*)
simplify_S2Evar
(x0: token, xs0: toks): (toks, toks, toks) = let
  val () = free_token(x0)
  val xs00 = drop_exn_free(xs0, 0)
  val (par, rest) = peek_paren_list3(xs00)
  val (par2, rest2) = takeskip_until_free(par, lam i => tok_chr_eq(i, '$'))
  val rest3 = skip_until_free(rest2, lam i => is_opr(i))
//
  val rest3 = drop_exn_free(rest3, 0)
  val (rest3, remove) = peek_paren_list3(rest3)
  val () = free_toks(remove)
in
  (par2, rest3, rest)
end


    (*
    | S2EVar of s2Var // existential variable
    *)

implmnt(*{}*)
simplify_S2EVar(x0, xs0) = let
  val (expression, rest) = simplify_whats_inside(xs0)
  val () = (free_token(x0); free_toks(rest))  
in
  expression
end


    (*
    | S2Ehole of s2hole // it used to form contexts
    *)


    (*
    | S2Edatcontyp of (* unfolded datatype *)
        (d2con, s2explst) (* constructor and types of arguments *)
    *)


    (*
    | S2Edatconptr of (* unfolded datatype *)
        (d2con, s2exp, s2explst) (* constructor and addrs of arguments *)
    *)

(*
implmnt(*{}*)
simplify_S2Edatconptr
(x0: token, xs0: toks): (toks, toks) = let
  val () = free_token(x0)
  val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
  val (h0, t0) = takeskip_until_free(s0, lam i => is_sco(i))
  // h0 ... for example 'list_vt_cons'
  val t0 = drop_exn_free(t0, 1)
  val (h1, t1) = takeskip_until_free(t0, lam i => is_sco(i))
  val t1 = drop_exn_free(t1, 1)
  val () = (print "datconptr( ";  print_toks_free_nonewln(h0))
in
  (h1, t1)
end
*)


    (*
    | S2Eat of (s2exp, s2exp) // for at-views
    *)

implmnt(*{}*)
simplify_S2Eat
(x0: token, xs0: toks): toks = let
  val () = free_token(x0)
  val rest = skip_until_in_free(xs0, lam i => is_opr(i))
  val () = print "@ "
in
  rest
end


    (*
    | S2Esizeof of (s2exp) // for sizes of types
    *)


    (*
    | S2Eeff of (s2eff) // effects
    *)


    (*
    | S2Eeqeq of (s2exp, s2exp) // generic static equality
    *)


    (*
    | S2Eproj of (s2exp(*addr*), s2exp(*type*), s2lablst) // projection
    *)


    (*
    | S2Eapp of (s2exp, s2explst) // static application
    *)

implmnt(*{}*)
simplify_S2Eapp
(x0: token, xs0: toks): (toks, toks, toks, toks, toks) = let
  val () = free_token(x0)
  val r0 = drop_exn_free(xs0, 0)
  val (r00, t00) = peek_paren_list(r0)
  val (h0, r1) = takeskip_until_free(r00, lam i => is_cpr(i))
    // now h0 is first cst
  val r2 = skip_until_free(r1, lam i => tok_is_s2e(i))
  val (r20, t20) = drop_head_tup(r2)
  val (r21, t20) = drop_head_tup(t20)
  val (par20, rpar20) = peek_paren_list(t20)
  val r2 = cons_vt(r20, cons_vt(r21, par20))
  val par11 = skip_until_free(rpar20, lam i => tok_is_s2e(i))
  val (par1, par1rest) = peek_paren_list3(par11)
  val (r30, t30) = drop_head_tup(r2)
  val (par30, rpar30) = peek_paren_list3(t30)
  val par = cons_vt(r30, par30)
  val () = free_toks(rpar30)
in
  (h0, par, par1, t00, par1rest)
end


    (*
    | S2Elam of (s2varlst, s2exp) // static abstraction
    *)


    (*
    | S2Efun of 
      ( // function type
        funclo, int(*lin*), s2eff, int(*npf*), s2explst(*arg*), s2exp(*res*))
    *)

implmnt(*{}*)
simplify_S2Efun
(x0: token, xs: toks): (toks, toks, toks, toks, toks) = let

  val () = free_token(x0)
  val rest = drop_exn_free(xs, 0)
  val (ftype, rest) = takeskip_until_free(rest, lam i => is_sco(i))
  val ftype = take_until_free2(ftype, lam i => not(is_ide(i)))
  
  val rest = drop_exn_free(rest, 1)
  val (lin, rest) = takeskip_until_free(rest, lam i => is_sco(i))
  val rest = drop_exn_free(rest, 1)
  val (eff, rest) = takeskip_until_free(rest, lam i => is_sco(i))
  val rest = drop_exn_free(rest, 1)
  val (npf, rest) = takeskip_until_free(rest, lam i => is_sco(i))
  val rest = drop_exn_free(rest, 1)
  val () = ( //free_toks(ftype); 
    free_toks(lin); free_toks(eff); free_toks(npf)
  )

  val (afun, annotate) = 
    takeskip_until_free(rest, lam i => is_col(i))
  val rev_afun = list_vt_reverse(afun)
  val rev_afun = skip_while_free(rev_afun, lam i => is_spc(i) || is_nwl(i))
  val (afun_head, rev_afun) = drop_head_tup(rev_afun) // drop trailling
  val () = free_token(afun_head)
  val (rev_return_type, rev_afun) = takeskip_until_in_free_rev(rev_afun, lam i => is_sco(i))

  val (rev_item, rev_return_type, rev_afun) = 
    (
      if head_is_pred(rev_afun, lam i => tok_chr_eq(i, ',')) then
        let
          val (rev_return_type2, rev_fun) = 
            takeskip_until_free(rev_afun, lam i => is_sco(i))
        in
            (rev_return_type, rev_return_type2, rev_fun)
        end
      else
        (nil_vt(), rev_return_type, rev_afun)
    ) : (toks, toks, toks)

  val item = list_vt_reverse(rev_item)
  val afun = list_vt_reverse(rev_afun)
  val return_type = list_vt_reverse(rev_return_type)

  val return_type = skip_while_free(return_type, lam i => is_sco(i) || is_spc(i))
  val annotate 
    = skip_while_free(annotate, lam i => is_col(i) || is_spc(i))
in
  (ftype, afun, item, return_type, annotate)
end


    (*
    | S2Emetfun of (stampopt, s2explst, s2exp) // metricked function
    *)


    (*
    | S2Emetdec of (s2explst(*met*), s2explst(*metbound*)) // strictly decreasing
    *)


    (*
    | S2Etop of (int(*knd*), s2exp) // knd: 0/1: topization/typization
    *)


    (*
    | S2Ewithout of (s2exp) // for a component taken out by the [view@] operation
    *)


    (*
    | S2Etyarr of (s2exp (*element*), s2explst (*dimension*))
    *)

implmnt(*{}*)
simplify_S2Etyarr
(x0: token, xs0: toks): (toks, toks) = let
  val () = free_token(x0)
  val xs0 = drop_exn_free(xs0, 0)
  //
  val (head, tail) = toks_head_tail_free(xs0)
  val (h1, t1) = peek_paren_list_opr(tail)
  val to_simplify = cons_vt(head, h1)
  val rest = skip_until_in_free(t1, lam i => is_spc(i))
  (* [S2Etyarr(
        S2Eapp(S2Ecst(g0int_t0ype);S2Eextkind(atstype_int));
        S2Eintinf(3)
     )] *)
in
  (to_simplify, rest)
end


    (*
    | S2Einvar of (s2exp) // it is a special type for handling type unification
            // HX: note that [S2Einvar] is *not* related to [S1Einvar];
    *)

implmnt(*{}*)
simplify_S2Einvar
(x0: token, xs0: toks): toks = let
  val () = free_token(x0)
  val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
  val () = print "invar "
in
  s0  
end


    (*
    | S2Etyrec of (tyreckind, int(*npf*), labs2explst) // tuple and record
    *)

implmnt(*{}*) 
simplify_S2Etyrec
(x0: token, xs0: toks): toks = let
  val () = free_token(x0)
  val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
  val (h1, t1) = toks_head_tail_free(s0)
  // h1 = flt or box
  val () = print_token0_free(h1)
  val () = print "("
  // val () = print_space();
  val next 
    = skip_until_free(t1, lam i => not(is_spc(i)) && not(is_sco(i)))
  //
  val (h2, t2) 
    = takeskip_until_free(next, lam i => is_spc(i) || is_sco(i))
  // h2 = npf...
  val () = free_toks(h2)
  //
  val next 
  = skip_until_free(t2, lam i => not(is_spc(i)) && not(is_sco(i)))
in
  next
end


    (*
    | S2Eexi of ( // exist. quantified type
              s2varlst(*vars*), s2explst(*props*), s2exp(*body*) )
    *)

implmnt(*{}*)
simplify_S2Eexi(x0, xs0): (toks, toks) = let
  val r0 = drop_exn_free(xs0, 0) // i.e. 'S2Eexi('
  val (t0,s0) = takeskip_until_in_free(r0, lam i => is_sco(i))
  // t0 = n$xxxx$xxxxx(xxxxx);
  // To ommit after $ ...
  val t0 = take_until_free2(t0, lam i => tok_chr_eq(i, '$')) 
  //
  val s1 = skip_until_free(s0, lam i => tok_is_s2e(i) || is_sco(i))
  (* val () = (remove_debug(s1, "s1")) *)
  val (res1, s3) = 
  (
    ifcase
    | head_is_pred(s1, lam i => is_sco(i)) => let
        val s3 = skip_until_free(s1, lam i => tok_is_s2e(i)) 
      in 
        (nil_vt(), s3) 
      end
    | _ => let
        val (h0, s2) = toks_head_tail_free(s1)
        // h0 = S2Eapp
        // s2 = (....;....,....); ......
        val (t3, s3) = peek_paren_list_opr(s2)
        // t3 = (...)
        // s3 = .... rest of
        val res1 = cons_vt(h0, t3)
      in
        (res1, s3)
      end
  ): (toks, toks)
  // repeat
  val s4 = skip_until_free(s3, lam i => tok_is_s2e(i))
  //
  val (h1, s5) = toks_head_tail_free(s4)
  // h0 = S2Eapp
  // s2 = (....;....,....); ......
  val (t6, s6) = peek_paren_list_opr(s5)
  val s6 = drop_exn_free(s6, 0)
  //
  val res2 = cons_vt(h1, t6)
  //
  (*
  val () = print("\n\ns6 = ")
  val () = print_toks_tokens(s6)
  val () = println!("\n\n")
  val () = free_toks(s6)
  *)
  val () = free_token(x0)
  val () = (
      print "["; print_toks_free_nonewln(t0); // if ommited after $
      print "] ";
  )
  val () = free_toks(res1)
in
  (res2, s6)
end


    (*
    | S2Euni of ( // universally quantified type
             s2varlst(*vars*), s2explst(*props*), s2exp(*body*))
    *)

implmnt(*{}*)
simplify_S2Euni
(x0: token, xs0: toks): (toks, toks, toks) = let
  val () = free_token(x0)
  val r0 = drop_exn_free(xs0, 0) // i.e. '('
  //
  val (t0,s0) = takeskip_until_in_free(r0, lam i => is_sco(i))
  // t0 = a$xxxx$xxxxx(xxxxx);
  // To ommit after $ ...
  val t0 = take_until_free2(t0, lam i => tok_chr_eq(i, '$'))
  //
  val (t1,s1) = takeskip_until_in_free(s0, lam i => is_sco(i))
  val t2 = skip_until_in_free(s1, lam i => is_spc(i))
in
  (t0, t1, t2)
end


    (*
    | S2Erefarg of (int(*0/1:val/ref*), s2exp) (* !/&: call-by-val/ref *)
    *)


    (*
    | S2Evararg of (s2exp) // variadic argument type
    *)


    (*
    | S2Ewthtype of (s2exp, wths2explst) // the result part of a fun type
    *)


    (*
    | S2Eerrexp of ((*void*)) 
                // HX: placeholder for indicating error or something else
    *)

        // see simplify
(* end of datatype s2exp_node *)


(* 
datatype s2rtbas
*)  
    (*
    | S2RTBASpre of (symbol) // predicative: int, bool, ...
    *)
//        
    (*
    | S2RTBASimp of (int(*knd*), symbol) // impredicative sorts
    *)
//
    (*
    | S2RTBASdef of (s2rtdat) // user-defined datasorts
    *)
//


(* 
datatype s2rt 
*)
    (*
    | S2RTfun of (s2rtlst, s2rt) // function sort
    *)
        // inside simplify_print
    (*
    | S2RTtup of s2rtlst (* tuple sort *)
    *)
//
    (*
    | S2RTVar of s2rtVar // HX: unification variable
    *)
//
    (*
    | S2RTerr of ((*void*)) // HX: error indication
    *)
//
    (*
    | S2RTbas of s2rtbas (* base sort *)
    *)
implmnt(*{}*)
simplify_S2RTbas
(x0: token, xs0: toks) : (toks, toks) = let
  // datatype s2rt = | S2RTbas of s2rtbas (* base sort *)
  val all = skip_until_in_free(xs0, lam i => is_sco(i))
  val all = drop_exn_free(all, 0)
  (* val res = take_until_free2(all, lam i => is_cpr(i)) *)
  val (res, rest) = takeskip_until_free(all, lam i => is_cpr(i))
  val rest = drop_exn_free(rest,1)
  val () = free_token(x0)
in
  (res, rest)
end





(* ****** ****** *) // begin Simplify Print


(*
extern
fn
simplify_print
(xs: toks, color: bool): void 
*)

implmnt(*{}*)
simplify_print(xs, color) = let
(*^^^TODO^^^*)

fun
auxmain(xs1: toks): void =
  case+ xs1 of 
  | ~nil_vt() => () //nil_vt()
  | ~cons_vt(x0, xs0) =>
    ifcase
    | is_spc(x0) => (print_token0_free(x0); auxmain(xs0)) (*  ' '  *)
    | is_csq(x0) => (free_token(x0); free_toks(xs0)) //auxmain(xs0)) (*  ']'  *)
    | is_opr(x0) => (print_token0_free(x0); auxmain(xs0)) (*  '('  *)
    | is_cpr(x0) => (print_token0_free(x0); auxmain(xs0)) (*  ')'  *)
    | is_int(x0) => (just_print(x0, color, "dim"); auxmain(xs0)) (*  int  *)
    | is_chr(x0) => (just_print(x0, color, "light_green"); auxmain(xs0)) (*  chr  *) 
    | is_col(x0) => (print_token0_free(x0); auxmain(xs0)) (*  ':'  *)
    | is_sco(x0) => (free_token(x0); auxmain(xs0)) (*  ';'  *)
    | is_err(x0) => (just_print(x0, color, "red"); auxmain(xs0)) (* 'error' *)
    | is_ide(x0) => (simplify_idestring(x0, color); auxmain(xs0)) (*  ide  *)
    (* datatype s2exp_node = *)
    | tok_s2e_eq(x0, "S2Eint") => simplify_S2Eint(x0, xs0)
    | tok_s2e_eq(x0, "S2Eintinf") => simplify_S2Eint(x0, xs0)
    | tok_s2e_eq(x0, "S2Efloat") => simplify_S2Efloat(x0, xs0)
    | tok_s2e_eq(x0, "S2Estring") => simplify_S2Estring(x0, xs0)
    | tok_s2e_eq(x0, "S2Ecst") => let 
          val (par, rest) = simplify_S2Ecst(x0, xs0)
        in  
          (auxmain(par); auxmain(rest))
        end
    | tok_s2e_eq(x0, "S2Eextype") => let
          val expression = simplify_S2Eextype(x0, xs0)
        in 
          auxmain(expression) 
        end
    | tok_s2e_eq(x0, "S2Eextkind") => let
          val expression = simplify_S2Eextkind(x0, xs0)
        in 
          auxmain(expression) 
        end
    | tok_s2e_eq(x0, "S2Evar") => let 
          val (par2, rest3, rest) = simplify_S2Evar(x0, xs0)
        in 
          (auxmain(par2); print " "; auxmain(rest3); auxmain(rest))
        end    
    | tok_s2e_eq(x0, "S2EVar") => let
        val res = simplify_S2EVar(x0, xs0)
      in
        auxmain(res)
      end
     // | tok_s2e_eq(x0, "S2Ehole") =>
     // | tok_s2e_eq(x0, "S2Edatacontyp") =>
     (* | tok_s2e_eq(x0, "S2Edatconptr") => let
        val (h1, t1) = simplify_S2Edatconptr(x0, xs0)
        in
          auxmain(h1); print "("; auxmain(t1); print " )";
        end *)
    | tok_s2e_eq(x0, "S2Eat") => auxmain(simplify_S2Eat(x0, xs0)) // 08-07
     // | tok_s2e_eq(x0, "S2Esizeof") =>
     // | tok_s2e_eq(x0, "S2Eeff") =>
     // | tok_s2e_eq(x0, "S2Eeqeq") =>
     // | tok_s2e_eq(x0, "S2Eproj") =>
    | tok_s2e_eq(x0, "S2Eapp") => let
        val (h0, par, par1, t00, par1rest) = simplify_S2Eapp(x0, xs0)
      in
        (
          auxmain(h0); print "("; auxmain(par); 
          (if isneqz par1 then  (print ", "; auxmain(par1)) else free_toks(par1));
          print ")"; auxmain(t00); auxmain(par1rest);
        )
      end
     // | tok_s2e_eq(x0, "S2Elam") =>
    | tok_s2e_eq(x0, "S2Efun") => let
          val (ftype, afun, item, return_type, annotate) 
            = simplify_S2Efun(x0, xs0)
        in
          (
            (* free_toks(ftype); *)
            print ("(");
            auxmain(afun); 
            print ") -> ("; auxmain(return_type); 
            (if isneqz item then (print "),"; auxmain(item)) else (print ")"; free_toks(item)));
            // experimental
            print "  :: [ "; print_toks_free_nonewln(ftype); 
            //
            (
              if isneqz(annotate) then 
                (print("  : "); auxmain(annotate))
              else (free_toks(annotate))
            );
            print " ]"
          )
        end
     // | tok_s2e_eq(x0, "S2Emetfun") =>
     // | tok_s2e_eq(x0, "S2Emetdec") =>
     // | tok_s2e_eq(x0, "S2Etop") =>
     // | tok_s2e_eq(x0, "S2Ewithout") =>
    | tok_s2e_eq(x0, "S2Etyarr") => let 
          val (to_simplify, rest) = simplify_S2Etyarr(x0, xs0)
        in
          (print("[ ["); auxmain(to_simplify); print("]["); auxmain(rest); print("] ]"))
        end
    | tok_s2e_eq(x0, "S2Etyrec") => let
        val next = simplify_S2Etyrec(x0, xs0)
      in
        auxmain(next);
      end
    | tok_s2e_eq(x0, "S2Einvar") => let
        val rest = simplify_S2Einvar(x0, xs0)            
      in
        auxmain(rest)
      end
    | tok_s2e_eq(x0, "S2Eexi") => let
          val (res2, rest) = simplify_S2Eexi(x0, xs0)
        in
          (auxmain(res2); auxmain(rest))
        end
    | tok_s2e_eq(x0, "S2Euni") => let
        val (t0, t1, t2) = simplify_S2Euni(x0, xs0)
      in
        print "uni. "; 
        auxmain(t0); auxmain(t1); auxmain(t2)
      end
     // | tok_s2e_eq(x0, "S2Evararg") =>
     // | tok_s2e_eq(x0, "S2Ewthtype") =>
     // | tok_s2e_eq(x0, "S2Erefarg") =>
    | tok_s2e_eq(x0, "S2Eerrexp") => 
        (
          (if color then (print_a_color("dim")));
          print_token0_free(x0);  
          (* print_toks_free_nonewln(xs0); *)
          free_toks(xs0);
          (if color then prcc)
        )
    (* end of [s2exp_node] *)

    (* datatype s2rtbas = *)
     // | tok_s2e_eq(x0, "S2RTBASpre") =>
     // | tok_s2e_eq(x0, "S2RTBASimp") =>
     // | tok_s2e_eq(x0, "S2RTBASdef") =>

    (* datatype s2rt = *)
    | tok_s2e_eq(x0, "S2RTbas") => let 
          val (res, rest) = simplify_S2RTbas(x0, xs0)
        in 
          auxmain(res); auxmain(rest)
        end

    | tok_s2e_eq(x0, "S2RTfun") => (free_token(x0); print "function sort "; auxmain(xs0))
     // | tok_s2e_eq(x0, "S2RTtup") =>
     // | tok_s2e_eq(x0, "S2RTVar") =>
     // | tok_s2e_eq(x0, "S2RTerr") =>

    (* Other - fallthrough *)
    | _ => (
        print_toks_free_nonewln(cons_vt(x0, xs0))
      )
in
  auxmain(xs)
end

(* ****** ****** *)

(* end of [simplify_print.dats] *)
