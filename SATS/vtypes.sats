staload "./token.sats"

// linear list of tokens
vtypedef toks
  = List0_vt(token)

// a (triple) tuple of toks
vtypedef toktup
  = @(toks, toks, toks)

// a linear list of toktups
vtypedef tokstup
  = List0_vt(toktup)

(* ****** ****** *)

// renaming for convenience 

typedef cfun(a:t@ype, b:t@ype)
  = a -<cloref1> b

vtypedef cfun_vt( a: vt@ype, b:t@ype) 
  = a -<cloref1> b

(* ****** ****** *)

