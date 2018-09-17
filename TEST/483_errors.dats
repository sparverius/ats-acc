(* ****** ****** *)
(*
** A Tokenizer
** based on linear streams
*)
(* ****** ****** *)

#include "./token.dats"
#include "./tokenize.dats"
#include "./token_lib.dats"
#include "./tok_vt.dats"
#include "./mylib/bashstr.dats"
#include "./mylib/mylib_orig.dats"

(* ****** ****** *)

#define nil list_nil
#define cons list_cons
#define foreach list_foreach
#define sing list_sing
vtypedef err_ent = List0_vt(List0_vt(token))
vtypedef toks = List0_vt(token)


fun
tok_eq_chr
(tok: !token, c: char): bool =
case+ tok of | TOKchr chr => chr = c | _ => false

fun
tok_eq_ide
(tok: !token, s: string): bool =
case+ tok of 
| TOKide ide => 
  let 
    val x0 = strnptr_copy(ide)
    val x1 = $UN.strnptr2string(x0)
    val res = x1 = s
    val () = strnptr_free(x0)
  in
    res
  end

| _ => false
fun
tok_eq_int
(tok: !token, i: string): bool =
case+ tok of 
| TOKint i0 => 
  let 
    val x0 = strnptr_copy(i0)
    val x1 = $UN.strnptr2string(x0)
    val res = x1 = i
    val () = strnptr_free(x0)
  in
    res
  end
| _ => false

fun
tok_eq_chr_int
(tok: !token, c: int): bool =
case+ tok of | TOKchr chr => char2int0(chr) = c | _ => false


overload tokeq with tok_eq_chr
overload = with tok_eq_chr


fun
is_chr
(tok: !token): bool =
case+ tok of | TOKchr _ => true | _ => false


fun
is_col
(tok: !token): bool =
case+ tok of | TOKcol _ => true | _ => false
fun
is_opr
(tok: !token): bool =
case+ tok of | TOKopr _ => true | _ => false
fun
is_cpr
(tok: !token): bool =
case+ tok of | TOKcpr _ => true | _ => false
fun
is_osq
(tok: !token): bool =
case+ tok of | TOKosq _ => true | _ => false
fun
is_csq
(tok: !token): bool =
case+ tok of | TOKcsq _ => true | _ => false
fun
is_spc
(tok: !token): bool =
case+ tok of | TOKspc _ => true | _ => false
fun
is_war
(tok: !token): bool =
case+ tok of | TOKwar _ => true | _ => false
fun
is_err
(tok: !token): bool =
case+ tok of | TOKerr _ => true | _ => false
fun
is_newln
(tok: !token): bool =
case+ tok of | TOKchr x => x = '\n' | _ => false
fun
is_int
(tok: !token): bool =
case+ tok of | TOKint _ => true | _ => false
fun
is_ide
(tok: !token): bool =
case+ tok of | TOKide _ => true | _ => false
fun
is_scol
(tok: !token): bool =
case+ tok of | TOKsco _ => true | _ => false
fun
tok_s2e_eq
(tok: !token, s: string): bool =
case+ tok of 
| TOKs2e x => //x = s 
  let 
    val x0 = strnptr_copy(x)
    val x1 = $UN.strnptr2string(x0)
    val res = x1 = s
    val () = strnptr_free(x0)
  in
    res
  end
| _ => false


(* ****** ****** *)

fun
tok_get_len
(tok: !token): int =
case+ tok of 
| TOKide ide => length(ide)
| TOKint itn => length(itn)
| TOKwar war => length(war)
| TOKerr err => length(err)
| TOKs2e s2e => length(s2e)
| TOKnil() => 0
| _ => 1

fun
peek_paren_depth
(xs: !toks): int = let
  fun auxmain
  (xs: !toks, np: int, res: int): int = //$tup(int, toks)) =
    case+ xs of
    | nil_vt() => res
    | cons_vt(x, xs) =>
      ifcase
        | np = 0 => res - 1 // subtract last ')' ... looking for inner depth
        | is_opr(x) => auxmain(xs, np+1, res+1)
        | is_cpr(x) => auxmain(xs, np-1, res+1)
        | (*else*)_ => auxmain(xs, np, res + tok_get_len(x))
  in
    auxmain(xs, 1, 0) // should start from after first paren
  end    

fun
peek_square_depth
(xs: !toks): int = let
  fun auxmain
  (xs: !toks, np: int, res: int): int = //$tup(int, toks)) =
    case+ xs of
    | nil_vt() => res
    | cons_vt(x, xs) =>
      ifcase
        | np = 0 => res - 1 // subtract last ')' ... looking for inner depth
        | is_osq(x) => auxmain(xs, np+1, res+1)
        | is_csq(x) => auxmain(xs, np-1, res+1)
        | (*else*)_ => auxmain(xs, np, res + tok_get_len(x))
  in
    auxmain(xs, 1, 0) // should start from after first paren
  end    


(* ****** ****** *)

fun
printall
(xs: !toks): void =
let
implement
(env)
list_vt_foreach$fwork<token><env>(c,env) = fprint_token(stdout_ref, c)
in
  list_vt_foreach(xs)
end

fun
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



fun
print_toks
(xs: !toks): void =
let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) = print_token0(c)
in
  list_vt_foreach(xs)
end

fun
print_toks
(xs: !toks): void =
let
  implement(env)
  list_vt_foreach$fwork<token><env>(c,env) = (if not(is_newln(c)) then print_token0(c))
in
  list_vt_foreach(xs)
end


(* ****** ****** *)

vtypedef toktup = (toks, toks, toks)

vtypedef tokstup = List0_vt(toktup)

fun
print_tokstup
(x: !toktup): void =
(
  print_toks(x.0); println!()
  ;
  print_toks(x.1); println!()
  ;
  print_toks(x.2); println!()
)


fun
print_all
(xs: !tokstup): void = let
fun
aux(xs: !tokstup): void =
case+ xs of
| cons_vt(x, xs) => 
  (print_toks(x.2); println!(); aux(xs))
| nil_vt() => println!()
in
  aux(xs)
end

fun
strstr_exists
(str1: string, str2: string): bool =
  if strstr(g1ofg0(str1), str2) = ~1 
    then false
  else (if string_length(str1) > 0 then true else false)


fun
print_int
(xs: !toks): void =
let
implement
(env)
list_vt_foreach$fwork<token><env>(c,env) = 
  (print "("; fprint_token(stdout_ref, c); 
    (if is_chr(c) then 
      case c of
      | TOKchr chr => (print " "; print (char2int0(chr)))
      | _ => print " "))
in
  list_vt_foreach(xs)
end



(* ****** ****** *)


extern
fun
token_all
(xs: !toks): List0_vt(toktup) //(tokstup, List0_vt(token))

datavtype errkind =
  | ERRwarn of toks // 
  | ERRparse of toks // error(parsing)
  | ERRtyleq of toks // mismatch of static terms (tyleq)
  | ERRdynexp of toks // 'the dynamic expression cannot be assigned the type
  | ERRdynvar of toks // 'the (non-linear) dynamic variable [xs$1123(-1)] is not avail
  | ERRcstpat of toks // 'the constructor pattern cannot be assigne the type [S2Eerrexp()]
  | ERRunsolv of toks // unsolved constraint C3NSTRprop(...)
  | ERRother of toks

extern
fun
print_errkind: print_vtype(errkind)
extern
fun
fprint_errkind: fprint_vtype(errkind)

overload print with print_errkind
overload fprint with fprint_errkind

(* ****** ****** *)

implement
print_errkind(err) =
fprint_errkind(stderr_ref, err)

implement
fprint_errkind(out, err) =
(
case+ err of
  | ERRwarn _ => fprint!(out, "ERRwarn(", "", ")")
  | ERRparse _ => fprint!(out, "ERRparse(", "", ")")
  | ERRother _ => fprint!(out, "ERRother(", "", ")")
  | ERRtyleq _ => fprint!(out, "ERRtyleq(", "", ")")
  | ERRdynexp _ => fprint!(out, "ERRdynexp(", "", ")")
  | ERRdynvar _ => fprint!(out, "ERRdynvar(", "", ")")
  | ERRcstpat _ => fprint!(out, "ERRcstpat(", "", ")")
  | ERRunsolv _ => fprint!(out, "ERRunsolv(", "", ")")
)

// classify err

fun
parse_three
(xs0: toks): errkind = let
fun
aux
(xs: toks): errkind =
case+ xs of 
| cons_vt(x, xs1) => 
  (
    let
      val ys0 = skip_until_in(xs, lam i => is_spc(i))
      (* val ys0 = list_skip_while(ys0, lam i => is_(i)) *)
      (* val () = (printall(ys0); println!("\n\n")) *)
      (* val ys1 = list_head_exn(ys0) *)
      val ys1 = toks_head(ys0)
    in
      ifcase
      | tok_eq_ide(ys1, "mismatch") => let
          val res = ERRtyleq(xs)
          val () = free_token(ys1)
          val () = free_toks(ys0)
        in
          res
        end
      | tok_eq_ide(ys1, "the") =>  
        let  
          val () = free_token(ys1)
          val-~Some_vt(cs) = drop_opt(ys0, 2)
          val ys2 = toks_head(cs)
          val () = free_toks(cs)
          val () = free_toks(ys0)
        in
          ifcase
          | is_opr(ys2) => 
            let 
              val res = ERRdynvar(xs)
              val () = free_token(ys2)
            in
              res
            end
          | tok_eq_ide(ys2, "constructor") => 
            let
              val res = ERRcstpat(xs)
              val () = free_token(ys2)
            in
              res
            end
          | _ => let
              val res = ERRdynexp(xs)
              val () = free_token(ys2)
            in
              res
            end
        end
      | tok_eq_ide(ys1, "unsolved") => let
          val res = ERRunsolv(xs)
          val () = free_token(ys1)
          val () = free_toks(ys0)
        in
          res
        end
      | _ => let
          val res = ERRother(xs)
          val () = free_token(ys1)
          val () = free_toks(ys0)
        in
          res
        end
    end
  )
| nil_vt() => ERRother(xs) // xs0)
in
  aux(xs0)
end

fun
when_err
(xs: toks): errkind =
(
let
  val y0 = drophead_opt(xs, 2)
  val-~Some_vt(y0) = y0
in
  ifcase
  | tok_eq_ide(y0, "parsing") => let
      val res = ERRparse(xs) 
      val () = free_token(y0)
    in
      res
    end
  | tok_eq_int(y0, "3") => let
      val () = free_token(y0)
      val res = parse_three(xs)
    in
      res
    end

  | _ => let
      val () = free_token(y0)
    in
      ERRother(xs)
    end
  end
)

vtypedef tokclass = List0_vt(errkind)

fun
classify_toks
(xs: tokstup): tokclass = let
fun
aux
(xs: tokstup, res: tokclass): tokclass =
case+ xs of
| cons_vt(x, xs) => 
(
  let
    val x2 = x.2
  in
    if isneqz x2 then
      ifcase
      | is_war(x2[0]) => 
        aux(xs, list_cons(ERRwarn(x.2), res)) 
      | is_err(x2[0]) => 
        aux(xs, list_cons(when_err(x.2), res))
      | _ => 
       let 
       in
        aux(xs, list_cons(ERRother(x.2), res)) 
       end
    else list_vt2t(list_reverse(res))
  end
)
| nil() => list_vt2t(list_reverse(res))
in
  aux(xs, nil)
end



fun
indentation
(i: int): void = let
fun
loop
(i: int): void =
  if i >(* = *) 0 then (print ' '; loop(i-1))
in
  loop(i)
end

fun
peek0
(ys: toks): token =
case+ ys of
| cons(x, xs) => x
| nil() => TOKnil()


fun
print_warn
(xs: toks): void = print_toks(xs)
fun
print_parse
(xs: toks): void = print_toks(xs)
fun
print_other
(xs: toks): void = print_toks(xs)
fun
print_tyleq
(xs: toks): void = print_toks(xs)
(* fun *)
(* print_dynvar *)
(* (xs: toks): void = print_toks(xs) *)

#define nl print("\n")
#define nl2 print("\n\n")


fun
pprint
(xs: toks): void = 
let
val spaces = 2
fun
aux(xs: toks, ident: int, last: token, o: int, cvar: bool): void =
case+ xs of
| cons(y, ys) =>
  (
  ifcase
  | tok_eq_chr(y, '.') =>
    aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o, o = 0)

  | is_opr(y) && not(is_cpr(peek0(ys))) && not(is_int(peek0(ys))) // =>
    && not(cvar) =>
    (print "\n"
    ; indentation(ident)
    ; print_token0(y)
    ; print "\n"
    ; indentation(ident+spaces)
    ; aux(ys, ident+spaces, y, if cvar then o+1 else 0, o = 0 )
    )
  | is_cpr(y) =>
    (
    if (is_opr(last) || is_int(last)) || (cvar && o != 0)
    then
    ( print_token0(y)
    ; aux
      (ys, ident, y, if cvar then o-1 else 0, if o = 0 then false else true)
    )
    else
    (* let val () = print!(" ** ") in *)
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if (cvar && o != 0) then o-1 else 0, cvar)
    )
    (* end *)
    )
  | is_scol(last) || tok_eq_chr(last,',') =>
    ( print "\n"
    ; indentation(ident-1)
    ; print_token0(y)
    ; aux(ys, ident, y, if cvar then o else 0, cvar)
    )
  | is_csq(y) && not(cvar) =>
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if cvar then o else 0, o = 0)
    )
  | tok_s2e_eq(y,"S2Evar") || tok_s2e_eq(y,"S2Ecst") ||
    tok_s2e_eq(y,"S2Einvar") =>
    (
      print_token0(y);
      aux(ys, ident, y, 0, true)
    )
  | is_osq(y) =>
    let
      val len = peek_square_depth(ys, is_osq, is_csq)
      val (take, skip, f0) =
        list_takeskip_until_in(ys, lam i => is_csq(i))
    in
    (
      ifcase
      | len > 30 - ident =>
        (
          print_token0(y); println!();
          aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o
            , if o = 0 then false else true)
        )
      | _ =>
        (
          print_token0(y);
          print_toks(take);
          aux(skip, ident, y, if cvar && is_opr(y) then o+1 else o
            , if o = 0 then false else true)
        )
    ); free(f0)
  end
  
  | _ =>
    ( print_token0(y)
//    ; (if is_osq(y) then println!())
    ; aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o
    , if o = 0 then false else true)
    )
  )
| nil => () //println!()
//end
in
  aux(xs, 0, TOKnil(), 0, false)
end


fun
print_tyleq
(xs: toks): void = //print_toks(xs)
let
  val (h0, t0, f0) = list_takeskip_until_in(xs, lam i => is_col(i))
  val (h1, t1, f1) = list_takeskip_until_in(t0, lam i => is_col(i))
  val (h2, t2, f2) = list_takeskip_until_in(t1, lam i => is_col(i))
  val (h3, t3, f3) = list_takeskip_until   (t2, lam i => tok_eq_ide(i, "The"))
  val (h4, t4, f4) = list_takeskip_until_in(t3, lam i => is_col(i))

  val h1 = list_drop_exn(h1, 1)
  val h3 = list_drop_exn(h3, 1)
  val t4 = list_drop_exn(t4, 1)


in
(

  print_toks(h0); nl; 
  prcc;
  (* prc blink; *)
  print_toks(h1); nl;
  print_toks(h2); nl;
  pprint(h3); nl;
//  prc ligt_green;
  print_toks(h4); nl;
  pprint(t4);
  
  free(f0); free(f1); free(f2); free(f3); free(f4);

)
end



fun
print_dynvar
(xs: toks): void = 
let
//  val () = (print "xs = "; printall(xs))

  val (head, tail, f0) = 
    list_takeskip_until_in(xs, lam i => is_col(i))

//  val () = (print "tail = "; print_toks(tail))

  val (head1, tail1, f1) = 
    list_takeskip_until(tail, lam i => is_osq(i))

  val head1 = 
    list_skip_while(head1, lam i => is_spc(i))

  val (head2, tail2, f2) = 
    list_takeskip_until_in(tail1, lam i => is_csq(i))  

  val head2 = 
    list_skip_while(head2, lam i => is_spc(i))

  val tail2 = 
    list_skip_while(tail2, lam i => is_spc(i))

in
  (
    print_toks(head);
    println!();
    print_toks(head1);
    nl;
    print_toks(head2);
    nl;
    print_toks(tail2);
    nl;
    free(f0); free(f1); free(f2)

  )
end

fun
print_cstpat
(xs: toks): void = //print_toks(xs)
let
  val x0 = list_take_until(xs, lam x => is_col(x)) // error(3)
  val xs = list_skip_until(xs, lam x => is_col(x))
  val xs = list_skip_until(xs, lam x => is_ide(x))

  val () = print_toks(x0)
  val () = println!()

  val x1 = list_take_until(xs, lam x => is_osq(x))
  val () = print_toks(x1)
  val () = print "\n"
  val xs = list_skip_until(xs, lam x => is_osq(x))

  val spaces = 2
fun
aux
(xs: toks, ident: int, last: token, o: int, cvar: bool): void =
let

in
case+ xs of
| cons(y, ys) =>
  (
  ifcase
  | tok_eq_chr(y, '.') =>   
    aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o, o = 0)

  | is_opr(y) && not(is_cpr(peek0(ys))) && not(is_int(peek0(ys))) // =>
    && not(cvar) =>
    (print "\n"
    ; indentation(ident)
    ; print_token0(y)
    ; print "\n"
    ; indentation(ident+spaces)
    ; aux(ys, ident+spaces, y, if cvar then o+1 else 0, o = 0 )
    )
  | is_cpr(y) =>
    (
    if (is_opr(last) || is_int(last)) || (cvar && o != 0)
    then 
    ( print_token0(y)
    ; aux
      (ys, ident, y, if cvar then o-1 else 0, if o = 0 then false else true)
    )
    else
    (* let val () = print!(" ** ") in *)
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if (cvar && o != 0) then o-1 else 0, cvar)
    )
    (* end *)
    )
  | is_scol(last) || tok_eq_chr(last,',') =>
    ( print "\n"
    ; indentation(ident-1)
    ; print_token0(y)
    ; aux(ys, ident, y, if cvar then o else 0, cvar)
    )
  | is_csq(y) && not(cvar) =>
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if cvar then o else 0, o = 0)
    )
  | tok_s2e_eq(y,"S2Evar") || tok_s2e_eq(y,"S2Ecst") ||
    tok_s2e_eq(y,"S2Einvar") =>
    (
    print_token0(y)
    ; aux(ys, ident, y, 0, true)
    )
  | is_osq(y) =>
    let
      val len = peek_square_depth(ys, is_osq, is_csq)
      val (take, skip, f0) = 
        list_takeskip_until_in(ys, lam i => is_csq(i))
    in
    (
      ifcase
      | len > 30 - ident =>
        (
          print_token0(y); println!();
          aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o
            , if o = 0 then false else true)
        )
      | _ =>
        (
          print_token0(y);
          print_toks(take);
          aux(skip, ident, y, if cvar && is_opr(y) then o+1 else o
            , if o = 0 then false else true)
        )
    ); free(f0)
    end
  | _ =>
    ( print_token0(y)
//    ; (if is_osq(y) then println!())
    ; aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o
    , if o = 0 then false else true)
    )
  )
| nil => () //println!()
end
in
  aux(xs, 0, TOKnil(), 0, false)
end

fun
print_dynexp
(xs: toks): void = print_cstpat(xs)


fun
print_unsolv
(xs: toks): void =  //print_toks(xs)
let
  val x0 = list_take_until(xs, lam x => is_col(x)) // error(3)
  val xs = list_skip_until(xs, lam x => is_col(x))

  val () = print_toks(x0)

  val xs = list_drop_exn(xs, 1)
  val x1 = list_take_until(xs, lam x => is_col(x)) // unsolved constraint
  val () = print_toks(x1)

  val xs = list_skip_until(xs, lam x => is_col(x))
  val xs = list_drop_exn(xs, 1)

  val xs = list_take_until(xs, lam x => tok_eq_ide(x, "typechecking")) // in case last
  val xs = list_drop_exn(xs, 1)
  val () = print!("\n\r")
  val spaces = 2
fun
aux
(xs: toks, ident: int, last: token, o: int, cvar: bool): void =
let
(* val () = print!(" ",o," ") *)
  (* val cvar = (o = 0) *)
  (* val () = print!(" ",o," ",cvar, " ") *)
in
case+ xs of
| cons(y, ys) =>
  (
  ifcase
  | tok_eq_chr(y, '.') =>   
    aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o, o = 0)

  | is_opr(y) && not(is_cpr(peek0(ys))) && not(is_int(peek0(ys))) // =>
    && not(cvar) =>
    (print "\n"
    ; indentation(ident)
    ; print_token0(y)
    ; print "\n"
    ; indentation(ident+spaces)
    ; aux(ys, ident+spaces, y, if cvar then o+1 else 0, o = 0 )
    )
  | is_cpr(y) =>
    (
    if (is_opr(last) || is_int(last)) || (cvar && o != 0)
    then 
    ( print_token0(y)
    ; aux
      (ys, ident, y, if cvar then o-1 else 0, if o = 0 then false else true)
    )
    else
    (* let val () = print!(" ** ") in *)
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if (cvar && o != 0) then o-1 else 0, cvar)
    )
    (* end *)
    )
  | is_scol(last) || tok_eq_chr(last,',') =>
    ( print "\n"
    ; indentation(ident-1)
    ; print_token0(y)
    ; aux(ys, ident, y, if cvar then o else 0, cvar)
    )
  | is_csq(y) && not(cvar) =>
    ( print "\n"
    ; indentation(ident-spaces)
    ; print_token0(y)
    ; aux(ys, ident-spaces, y, if cvar then o else 0, o = 0)
    )
  | tok_s2e_eq(y,"S2Evar") || tok_s2e_eq(y,"S2Ecst") =>
    (
    print_token0(y)
    ; aux(ys, ident, y, 0, true)
    )
  | _ =>
    ( print_token0(y)
    ; (if is_osq(y) then println!())
    ; aux(ys, ident, y, if cvar && is_opr(y) then o+1 else o
    , if o = 0 then false else true)
    )
  )
| nil => () //println!()
end
in
  aux(xs, 0, TOKnil(), 0, false)
end

fun
print_classified
(xs: tokclass): void = 
let
fun
aux
(xs: tokclass): void =
case+ xs of
| cons(x, xs) => (
  (
  case+ x of
  | ERRwarn x =>   (println!("ERRwarn"); print_warn(x); nl2)
  | ERRparse x =>  (println!("ERRparse"); print_parse(x); nl2)
  | ERRother x =>  (println!("ERRother"); print_other(x); nl2)
  | ERRtyleq x =>  (println!("ERRtyleq"); print_tyleq(x); nl2)
  | ERRdynexp x => (println!("ERRdynexp"); print_dynexp(x); nl2)
  | ERRdynvar x => (println!("ERRdynvar"); print_dynvar(x); nl2)
  | ERRcstpat x => (println!("ERRcstpat"); print_cstpat(x); nl2)
  | ERRunsolv x => (println!("ERRunsolv"); print_unsolv(x); nl2)
  )(* ; print "\n\n" *); aux(xs))
| nil => ()(* println!() *)
in
  aux(xs)
end

(* ****** ****** *)
