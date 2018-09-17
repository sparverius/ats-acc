(* ****** ****** *)


datavtype token =
  | TOKide of Strnptr // ide=alpha[alnum]*
  | TOKint of Strnptr // int=digit[digit]*
  | TOKchr of (char)  // special character
  //
  | TOKwar of Strnptr // warning
  | TOKerr of Strnptr // error
  | TOKs2e of Strnptr // S2E...
  //
  | TOKcol of (char)  // : colon
  | TOKsco of (char)  // ; semi-colon
  | TOKopr of (char)  // ( open paren
  | TOKcpr of (char)  // ) close paren
  | TOKosq of (char)  // [ open square bracket
  | TOKcsq of (char)  // ] close square bracket
  | TOKspc of (char)  // ' '
  //
  | TOKnil of ((**))  // unit  


(* ****** ****** *)


fun print_token: print_vtype(token)
fun fprint_token: fprint_vtype(token)

overload print with print_token
overload fprint with fprint_token


(* ****** ****** *)

fn
free_token
(x : token): void

fn
create_token
(!token): token
