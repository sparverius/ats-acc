(* ****** ****** *)

staload "./../SATS/token.sats"

(* ****** ****** *)


implement print_token(tok) = 
  fprint_token(stdout_ref, tok)


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
  | TOKnil()    => fprint!(out, "TOKnil")  
)


(* ****** ****** *)


implement
free_token(x): void = 
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
      | ~TOKnil  _  => ((* x *))
  )
}


implement
create_token(x) = 
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

(* end of [token.dats] *)
