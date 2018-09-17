staload "./vtypes.sats"
staload "./errkind.sats"

(**
  separates the tokens into messages
*)
fn
tokens_to_messages_free
(xs: toks): List0_vt(toktup)

(**
  parses the 'error(3)' messages
*)
fn
parse_three
(xs: toktup): errtup

(**
  parses the message when it starts with "error..."
*)
fn
when_err
(xs: toktup): errtup

(**
  classifies the first 'n' messages
*)
fn
classify_toks_free_n
(xs: tokstup, n: int): errtups

(**
  classifies the token messages
*)
fn
classify_toks_free
(xs: tokstup): errtups
