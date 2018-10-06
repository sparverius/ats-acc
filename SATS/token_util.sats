staload "./token.sats"

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
