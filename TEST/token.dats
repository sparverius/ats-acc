(* ****** ****** *)


datavtype token =
  | TOKide of Strnptr // ide=alpha[alnum]*
  | TOKint of Strnptr // int=digit[digit]*
  | TOKchr of (char)  // special character
  | TOKwar of Strnptr // warning
  | TOKerr of Strnptr // error
  | TOKs2e of Strnptr // S2E...
  | TOKcol of (char) // : colon
  | TOKsco of (char) // ; semi-colon
  | TOKopr of (char) // ( open paren
  | TOKcpr of (char) // ) close paren
  | TOKosq of (char) // [ open square bracket
  | TOKcsq of (char) // ] close square bracket
  | TOKspc of (char) // ' '
  | TOKnil of () // ' '  


(* ****** ****** *)


extern fun print_token: print_vtype(token)
extern fun fprint_token: fprint_vtype(token)

overload print with print_token
overload fprint with fprint_token


(* ****** ****** *)


implement print_token(tok) = fprint_token(stdout_ref, tok)

implement fprint_token(out, tok) = 
(
  case+ tok of
  | TOKint(int) => fprint!(out, "TOKint(", int, ")")
  | TOKide(ide) => fprint!(out, "TOKide(", ide, ")")  
  | TOKchr(chr) => fprint!(out, "TOKchr(", chr, ")")  
  | TOKs2e(s2e) => fprint!(out, "TOKs2e(", s2e, ")")  
  | TOKwar(war) => fprint!(out, "TOKwar(", war, ")")  
  | TOKerr(err) => fprint!(out, "TOKerr(", err, ")")  
  | TOKcol(col) => fprint!(out, "TOKcol(", col, ")")  
  | TOKsco(col) => fprint!(out, "TOKsco(", col, ")")  
  | TOKopr(col) => fprint!(out, "TOKopr(", col, ")")  
  | TOKcpr(col) => fprint!(out, "TOKcpr(", col, ")")  
  | TOKosq(col) => fprint!(out, "TOKosq(", col, ")")  
  | TOKcsq(col) => fprint!(out, "TOKcsq(", col, ")")  
  | TOKspc(spc) => fprint!(out, "TOKspc()")  
  | TOKnil() =>  fprint!(out, "TOKnil")  
)


(* ****** ****** *)


extern fn free_token(x : token): void
implement free_token(x): void = 
{
  val _ = (
    case+ x of
      | ~TOKide (s) => strnptr_free(s)
      | ~TOKint (s) => strnptr_free(s)
      | ~TOKs2e (s) => strnptr_free(s)
      | ~TOKwar (s) => strnptr_free(s)
      | ~TOKerr (s) => strnptr_free(s)
      | ~TOKchr (_) => ((* x *))
      | ~TOKcol (_) => ((* x *))
      | ~TOKsco (_) => ((* x *))
      | ~TOKopr (_) => ((* x *))
      | ~TOKcpr (_) => ((* x *))
      | ~TOKosq (_) => ((* x *))
      | ~TOKcsq (_) => ((* x *))
      | ~TOKspc (_) => ((* x *))
      | ~TOKnil _ => ((* x *))
  )
}


(* ****** ****** *)


extern fn create_token(!token): token
implement create_token(x) = 
(
  case+ x of 
  | TOKide s => let val x3 = strnptr_copy(s) in TOKide(x3) end
  | TOKint s => let val x3 = strnptr_copy(s) in TOKint(x3) end
  | TOKchr c => TOKchr(c)
  | TOKs2e(s2e) => let val x3 = strnptr_copy(s2e) in TOKs2e(x3) end
  | TOKwar(war) => let val x3 = strnptr_copy(war) in TOKwar(x3) end
  | TOKerr(err) => let val x3 = strnptr_copy(err) in TOKerr(x3) end
  | TOKcol(col) => TOKcol(col)
  | TOKsco(sco) => TOKsco(sco)
  | TOKopr(opr) => TOKopr(opr)
  | TOKcpr(cpr) => TOKcpr(cpr)
  | TOKosq(osq) => TOKosq(osq)
  | TOKcsq(csq) => TOKcsq(csq)
  | TOKspc(spc) => TOKspc(spc)
  | TOKnil() => TOKnil()

) 


(* ****** ****** *)


extern fn create_token_free(x: token): token
implement create_token_free(x) = ( let 
  val xy = (
    case+ x of 
    | ~TOKide(ide) => let val str = strnptr_copy_free(ide) in TOKide(str) end
    | ~TOKint(itn) => let val str = strnptr_copy_free(itn) in TOKint(str) end
    | ~TOKs2e(s2e) => let val str = strnptr_copy_free(s2e) in TOKs2e(str) end
    | ~TOKwar(war) => let val str = strnptr_copy_free(war) in TOKwar(str) end
    | ~TOKerr(err) => let val str = strnptr_copy_free(err) in TOKerr(str) end
    | ~TOKchr(chr) => let (**) in TOKchr(chr) end
    | ~TOKcol(col) => let (**) in TOKcol(col) end
    | ~TOKsco(sco) => let (**) in TOKsco(sco) end
    | ~TOKopr(opr) => let (**) in TOKopr(opr) end
    | ~TOKcpr(cpr) => let (**) in TOKcpr(cpr) end
    | ~TOKosq(osq) => let (**) in TOKosq(osq) end
    | ~TOKcsq(csq) => let (**) in TOKcsq(csq) end
    | ~TOKspc(spc) => let (**) in TOKspc(spc) end
    | ~TOKnil()    => let (**) in TOKnil()    end
) : token
in
  xy
end where 
{
fn strnptr_copy_free(s: Strnptr): Strnptr = let
  val res = strnptr_copy(s)
  val () = strnptr_free(s)
in
  res
end
}
)


(* ****** ****** *)



