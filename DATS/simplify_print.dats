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

(* #include "./pprint.dats" *)

(* ****** ****** *)


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

    y <~ x  :  x should be y 
   y <?~ x  :  x should (most likely) be y    
      
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

     XXXX   :  where X is an integer (I believe is a tag of some sort)
     XXXXX       
*)

extern
fn
simplify_print
(xs: toks, color: bool): void 

implement
simplify_print(xs, color) = let
(*^^^TODO^^^*)
  fun
  auxmain(xs1: toks): void =
    case+ xs1 of 
    | ~nil_vt() => () //nil_vt()
    | ~cons_vt(x0, xs0) =>
        ifcase
  (*  ' '  *)
        | is_spc(x0) => (print_token0_free(x0); auxmain(xs0))
  (*  ']'  *)
        | is_csq(x0) => (free_token(x0); free_toks(xs0)) //auxmain(xs0))
  (*  '('  *)
        | is_opr(x0) => (print_token0_free(x0); auxmain(xs0))
        (* free_token(x0); free_toks(xs0)) //auxmain(xs0)) *)
  (*  ')'  *)
        | is_cpr(x0) => (print_token0_free(x0); auxmain(xs0))
        (* (free_token(x0); free_toks(xs0)) //auxmain(xs0)) *)
  (*  int  *)
        | is_int(x0) => (
            (if color then print_a_color("dim"));
            print_token0_free(x0); 
            (if color then prcc);            
            auxmain(xs0)
          )
  (*  chr  *)
        | is_chr(x0) => (
            (if color then print_a_color("light_green"));
            print_token0_free(x0); 
            (if color then prcc);
            auxmain(xs0)
          )
  (*  ':'  *)
        | is_col(x0) => (print_token0_free(x0); auxmain(xs0))
  (*  ';'  *)
        | is_sco(x0) => (free_token(x0); auxmain(xs0))
           // | is_sco(x0) => (print_token0_free(x0); auxmain(xs0))
  (*  ide  *)
        | is_ide(x0) => (
          case+ x0 of
          | ~TOKide(i) => let
              val _ = assertloc(g1int2int_ssize_int(length(i)) > 0)
              val str = $UN.strnptr2string(i)
              val () = (if color then print_a_color("cyan"))
              val () = 
                (
                  case+ str of
                  | "g0int_t0ype" => print "g0int"
                  | "g0float_t0ype" => print "g0float"
                  | "g0uint_t0ype" => print "g0uint"
                  | "g1int_int_t0ype" => print "g1int"
                  | "char_t0ype" => print "char"
                  | "char_int_t0ype" => print "char"
                  | "string_type" => print "string"
                  | "string_int_type" => print "string"// even though indexed?
                  | "strnptr_addr_int_vtype" => print "strnptr"
                  | "list_t0ype_int_type" => print "list"
                  | "list_vt0ype_int_vtype" => print "list_vt"
                  | "atstype_int" => (
                      (if color then (print_a_color("dim"); print_a_color("yellow")));
                      print "atstype_int";
                      (if color then prcc)
                    )
                  | "int_kind" => (
                      (if color then (print_a_color("dim"); print_a_color("yellow")));
                      print "int_kind";
                      (if color then prcc)
                    )

                  (* | "sub_int_int" => print "- " *)
                  (* | "add_int_int" => print "+ " *)
                  | _ => (
                    if color then ((* print_a_color("reset_all"); *) (* print_a_color("dim"); *) print i; prcc)
                    else print i
                  )
                )
              val () = (if color then prcc)
              val () = strnptr_free(i)
            in
              auxmain(xs0)
            end
          | _ => 
              (assertloc (1 < 0); print_token0_free(x0); auxmain(xs0)) 
              // error
         )
(*
   from *** ATS2/src/pats_staexp2.sats *** (line 477-557 and ~559-~582)

   datatype
     s2exp_node =
*)

    (*
    | S2Eint of int // integer
    *)
        // NOTE: could use one for s2eint/intinf/float
        | tok_s2e_eq(x0, "S2Eint") => let
              val xs00 = drop_exn_free(xs0, 0)
              val (par, rest) = peek_paren_list3(xs00)
            in 
              ( 
                free_token(x0); free_toks(rest); 
                print_toks_free_nonewln(par); 
              )
            end    
    (*
    | S2Eintinf of intinf // integer of flex precision
    *)
        | tok_s2e_eq(x0, "S2Eintinf") => let
              val xs00 = drop_exn_free(xs0, 0)
              val (par, rest) = peek_paren_list3(xs00)
            in 
              ( free_token(x0); 
                free_toks(rest); 
                print "inf ";
                print_toks_free_nonewln(par); 
                (* print " [inf]" *)
                )
            end    
    (*
    | S2Efloat of string // static floating-points
    *)
        // | tok_s2e_eq(x0, "S2Efloat") =>
    (*
    | S2Estring of string // static string constants
    *)
        // | tok_s2e_eq(x0, "S2Estring") =>
    (*
    | S2Ecst of s2cst // constant
    *)
        | tok_s2e_eq(x0, "S2Ecst") => let
              val xs00 = drop_exn_free(xs0, 0)
              val (par, rest) = peek_paren_list3(xs00)
            in 
              (
                free_token(x0); 
                auxmain(par); 
                auxmain(rest)
              )
            end

    (*
    | S2Eextype of (string(*name*), s2explstlst) // external type
    | S2Eextkind of (string(*name*), s2explstlst) // external tkind
    *)
        | tok_s2e_eq(x0, "S2Eextype" (*assumed to be the same as extkind*)) ||
          tok_s2e_eq(x0, "S2Eextkind") => let
            val () = free_token(x0)
            val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
            val (par, rest) = peek_paren_list3(s0)
            val () = print "# "
            // val () = print "ext. "
            val () = free_toks(rest)
          in
            auxmain(par)
          end
    (*
    | S2Evar of s2var // universal variable
    *)
        | tok_s2e_eq(x0, "S2Evar") => let 
              val xs00 = drop_exn_free(xs0, 0)
              val (par, rest) = peek_paren_list3(xs00)
              val (par2, rest2) = takeskip_until_free(par, lam i => tok_chr_eq(i, '$'))
              val rest3 = skip_until_free(rest2, lam i => is_opr(i))
            in 
              (
                free_token(x0);
                (* print!("par2 = "); *)
                (* print_toks_free_nonewln(par2); *)
                auxmain(par2);
                (* print!(" rest3 = "); *)
                auxmain(rest3);
                (* print_toks_free_nonewln(rest3); *)
                (* print!("<< "); *)


                auxmain(rest);
                (* print " [v]" *)
              )
            end    
    (*
    | S2EVar of s2Var // existential variable
    *)
        | tok_s2e_eq(x0, "S2EVar") => let 
              val xs00 = drop_exn_free(xs0, 0)
              val (par, rest) = peek_paren_list3(xs00)
            in 
              (
                free_token(x0); free_toks(rest); 
                auxmain(par)
              )
            end    
    (*
    | S2Ehole of s2hole // it used to form contexts
    *)
        // | tok_s2e_eq(x0, "S2Ehole") =>
    (*
    | S2Edatcontyp of (* unfolded datatype *)
        (d2con, s2explst) (* constructor and types of arguments *)
    *)
        // | tok_s2e_eq(x0, "S2Edatacontyp") =>
    (*
    | S2Edatconptr of (* unfolded datatype *)
        (d2con, s2exp, s2explst) (* constructor and addrs of arguments *)
    *)
        (*
        | tok_s2e_eq(x0, "S2Edatconptr") =>
            (* (print_token0_free(x0);  print_toks_free_nonewln(xs0)) *)
          let
            val () = free_token(x0)
            val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
            val (h0, t0) = takeskip_until_free(s0, lam i => is_sco(i))
            // h0 ... for example 'list_vt_cons'
            val t0 = drop_exn_free(t0, 1)
            val (h1, t1) = takeskip_until_free(t0, lam i => is_sco(i))
            val t1 = drop_exn_free(t1, 1)

          in
            print "datconptr( ";
            print_toks_free_nonewln(h0);
            auxmain(h1);
            print "(";
            auxmain(t1);
            print " )";
          end
          *)

    (*
    | S2Eat of (s2exp, s2exp) // for at-views
    *)
        // 08-07
        | tok_s2e_eq(x0, "S2Eat") => 
            (* (print_token0_free(x0);  print_toks_free_nonewln(xs0)) *)
          let
            val () = free_token(x0)
            val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
            val () = print "@ "
          in
            auxmain(s0)
          end
          // end 08-07
    (*
    | S2Esizeof of (s2exp) // for sizes of types
    *)
        // | tok_s2e_eq(x0, "S2Esizeof") =>
    (*
    | S2Eeff of (s2eff) // effects
    *)
        // | tok_s2e_eq(x0, "S2Eeff") =>
    (*
    | S2Eeqeq of (s2exp, s2exp) // generic static equality
    *)
        // | tok_s2e_eq(x0, "S2Eeqeq") =>
    (*
    | S2Eproj of (s2exp(*addr*), s2exp(*type*), s2lablst) // projection
    *)
        // | tok_s2e_eq(x0, "S2Eproj") =>
    (*
    | S2Eapp of (s2exp, s2explst) // static application
    *)
        | tok_s2e_eq(x0, "S2Eapp") => let
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
            (
              auxmain(h0);
              print "(";
              auxmain(par); 
              ( 
                if isneqz par1 then
                (
                  print ", ";
                  auxmain(par1)
                ) 
                else free_toks(par1)
              );
              print ")";
              auxmain(t00);
              auxmain(par1rest);
            )
          end

    (*
    | S2Elam of (s2varlst, s2exp) // static abstraction
    *)
        // | tok_s2e_eq(x0, "S2Elam") =>
    (*
    | S2Efun of ( // function type
                funclo
                , int(*lin*), s2eff, int(*npf*), s2explst(*arg*), s2exp(*res*)
              ) (* end of S2Efun *)
    *)

        | tok_s2e_eq(x0, "S2Efun") => let
              val () = free_token(x0)
              val xs0 = drop_exn_free(xs0, 0)
              
            (*
              val (ftype, rest) = 
                takeskip_until_free(xs0, lam i => is_sco(i))
              val rest = drop_exn_free(xs0, 1)
              
              val (lin, rest) = 
                takeskip_until_free(xs0, lam i => is_sco(i))
              val rest = drop_exn_free(xs0, 1)
              
              val (eff, rest) = 
                takeskip_until_free(xs0, lam i => is_sco(i))
              val rest = drop_exn_free(xs0, 1)
            *)
              val (afun, annotate) = 
                takeskip_until_free(xs0, lam i => is_col(i))
                
              val annotate 
                = skip_while_free(annotate, lam i => is_col(i) || is_spc(i))
            in
              (
                print_toks_free(afun);
                print_ident6;
                print ": ";
                auxmain(annotate)
              )
            end
    (*
    | S2Emetfun of (stampopt, s2explst, s2exp) // metricked function
    *)
        // | tok_s2e_eq(x0, "S2Emetfun") =>
    (*
    | S2Emetdec of (s2explst(*met*), s2explst(*metbound*)) // strictly decreasing
    *)
        // | tok_s2e_eq(x0, "S2Emetdec") =>
    (*
    | S2Etop of (int(*knd*), s2exp) // knd: 0/1: topization/typization
    *)
        // | tok_s2e_eq(x0, "S2Etop") =>
    (*
    | S2Ewithout of (s2exp) // for a component taken out by the [view@] operation
    *)
        // | tok_s2e_eq(x0, "S2Ewithout") =>
    (*
    | S2Etyarr of (s2exp (*element*), s2explst (*dimension*))
    *)
        | tok_s2e_eq(x0, "S2Etyarr") => let 
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
            print("[ "); print("[");
            auxmain(to_simplify);
            print("][");
            auxmain(rest);
            print("]"); print(" ]");
          end
    (*
    | S2Etyrec of (tyreckind, int(*npf*), labs2explst) // tuple and record
    *)
        | tok_s2e_eq(x0, "S2Etyrec") => let
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
            auxmain(next);
          end
      
    (*
    | S2Einvar of (s2exp) // it is a special type for handling type unification
            // HX: note that [S2Einvar] is *not* related to [S1Einvar];
    *)
        | tok_s2e_eq(x0, "S2Einvar") => 
          let
            val () = free_token(x0)
            val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
            val () = print "invar "
          in
            auxmain(s0)
          end
    (*
    | S2Eexi of ( // exist. quantified type
              s2varlst(*vars*), s2explst(*props*), s2exp(*body*) )
    *)
        | tok_s2e_eq(x0, "S2Eexi") => let
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
            //
            val res2 = cons_vt(h1, t6)
            //
            val () = free_toks(s6)
            val () = free_token(x0)
          in
            // print "("; // to enclose in paren
            print "[";
            print_toks_free_nonewln(t0);
            // if ommited after $
            print "] ";
            (* print " ( "; *)
            (* print "par= "; *) // TO PRINT IDENTIFIER  i.e. [par= S2Ecst(token)] use .
            // print_toks_free_nonewln(par); 
            // TO PRINT EXPR i.e. [S2Ecst(token)] use .
            // auxmain(res1); //*** // TO PRINT INSIDE EXPR i.e. [token] use.
            free_toks(res1);
            // print "par1= "; // TO PRINT IDENTI i.e. [par= S2Ecst(token)]
            (* print_toks_free_nonewln(par1); *)  // TO PRINT EXPR i.e. [S2Ecst(token)] use .
            auxmain(res2); // TO PRINT INSIDE EXPR i.e. [token] use.;
            (* (remove_debug(res2, "res2"); *)
          end
    (*
    | S2Euni of ( // universally quantified type
             s2varlst(*vars*), s2explst(*props*), s2exp(*body*))
    *)
        | tok_s2e_eq(x0, "S2Euni") => let
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
            print "uni. "; 
            auxmain(t0); auxmain(t1); auxmain(t2)
          end

    (*
    | S2Erefarg of (int(*0/1:val/ref*), s2exp) (* !/&: call-by-val/ref *)
    *)
        // | tok_s2e_eq(x0, "S2Erefarg") =>
    (*
    | S2Evararg of (s2exp) // variadic argument type
    *)
        // | tok_s2e_eq(x0, "S2Evararg") =>
    (*
    | S2Ewthtype of (s2exp, wths2explst) // the result part of a fun type
    *)
        // | tok_s2e_eq(x0, "S2Ewthtype") =>
    (*
    | S2Eerrexp of ((*void*)) 
            // HX: placeholder for indicating error or something else
    *)
        | tok_s2e_eq(x0, "S2Eerrexp") => 
            (
              (if color then (print_a_color("dim")));
              print_token0_free(x0);  
              (* print_toks_free_nonewln(xs0); *)
              free_toks(xs0);
              (if color then prcc)
            )
  (*
    // end of [s2exp_node]
  *)

(*
    datatype s2rtbas =
*)
    (*
    | S2RTBASpre of (symbol) // predicative: int, bool, ...
    *)
        // | tok_s2e_eq(x0, "S2RTBASpre") =>
    (*
    | S2RTBASimp of (int(*knd*), symbol) // impredicative sorts
    *)
        // | tok_s2e_eq(x0, "S2RTBASimp") =>
    (*
    | S2RTBASdef of (s2rtdat) // user-defined datasorts
    *)
        // | tok_s2e_eq(x0, "S2RTBASdef") =>


(*
  datatype s2rt =
*)

    (*
    | S2RTbas of s2rtbas (* base sort *)
    *)
        | tok_s2e_eq(x0, "S2RTbas") => let 
          //    datatype s2rt = | S2RTbas of s2rtbas (* base sort *)
              val all = skip_until_in_free(xs0, lam i => is_sco(i))
              (* val () = remove_debug(all, "all") *)
              val all = drop_exn_free(all, 0)
              (* val () = remove_debug(all, "all") *)
              val res = take_until_free2(all, lam i => is_cpr(i))
              (* val () = remove_debug(rest, "res") *)
              val () = free_token(x0)
            in 
              auxmain(res)
            end
    (*
    | S2RTfun of (s2rtlst, s2rt) // function sort
    *)
         // | tok_s2e_eq(x0, "S2RTfun") =>
    (*
    | S2RTtup of s2rtlst (* tuple sort *)
    *)
         // | tok_s2e_eq(x0, "S2RTtup") =>
    (*
    | S2RTVar of s2rtVar // HX: unification variable
    *)
         // | tok_s2e_eq(x0, "S2RTVar") =>
    (*
    | S2RTerr of ((*void*)) // HX: error indication
    *)
         // | tok_s2e_eq(x0, "S2RTerr") =>


(*
  tok_is_s2e // 08-07
*)
(*
        | tok_is_s2e(x0) => let
              val () = free_token(x0)
              val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
              val (h0, t0) = takeskip_until_in_free(s0, lam i => is_sco(i))
            (* val () = print "dconptr " *)
            in
              print_toks_free_nonewln(h0);
              print "(";
              auxmain(t0);
              print "\n>>>>>>>> S2E?"
            end
*)
      // end 08-07
        (*
        | tok_is_s2e(x0) => let
            val () = free_token(x0)
            val s0 = skip_until_in_free(xs0, lam i => is_opr(i))
            val (par, rest) = peek_paren_list3(s0)
            val () = free_toks(rest)
          in
            auxmain(par)
          end
        *)
(*
  Other - fallthrough
*)
        | _ => (print_toks_free(cons_vt(x0, xs0)))
            // (pprint_free(cons_vt(x0, xs0))) 
            // print "\nOTHER\n"
in
  auxmain(xs)
end

(* ****** ****** *)

(* end of [simplify_print.dats] *)
