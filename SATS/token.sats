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

