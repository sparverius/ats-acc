(* ****** ****** *)

(*
implement{}
get_simplified_idestring(idestring) = ""
implement{}
print_simplified_idestring(idestring, color_name, color) = ()
implement{}
just_print(x0, color, color_name) = (free_token(x0))
implement{}
simplify_whats_inside(xs0) = let val () = free_toks(xs0) in (nil_vt(), nil_vt()) end
implement{}
generic_simplify(x0, xs0, insert) = (free_token(x0); free_toks(xs0))
*)


(* ****** ****** *)

(*
implement{}
simplify_idestring(x0, color) = (free_token(x0))

(* ****** ****** *)
(* 
datatype s2exp_node
*)
implement{}
simplify_S2Eint(x0, xs0) = (free_token(x0); free_toks(xs0))
implement{}
simplify_S2Eintinf(x0, xs0) = (free_token(x0); free_toks(xs0))
implement{} 
simplify_S2Efloat(x0, xs0) = (free_token(x0); free_toks(xs0))
implement{} 
simplify_S2Estring(x0, xs0) = (free_token(x0); free_toks(xs0))
implement{}
simplify_S2Ecst(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
(* helper *)  
implement{}
simplify_S2Eext(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2Eextype(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end // simplify_S2ext(x0, xs0)
implement{}
simplify_S2Eextkind(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end // simplify_S2ext(x0, xs0)
implement{}
simplify_S2Evar(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt(), nil_vt()) end
implement{} 
simplify_S2EVar(x0, xs0) = 
(free_token(x0); free_toks(xs0))
implement{}
simplify_S2Ehole(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Edatcontyp(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Edatconptr(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Eat(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_sizeof(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2Eeff(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2Eeqeq(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Eproj(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Eapp(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) 
in (nil_vt(), nil_vt(), nil_vt(), nil_vt(), nil_vt()) end
implement{}
simplify_S2Elam(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Efun(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) 
in (nil_vt(), nil_vt(), nil_vt(), nil_vt(), nil_vt()) end
implement{}
simplify_S2Emetfun(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Emetdec(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Etop(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Ewithout(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Etyarr(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Einvar(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{} 
simplify_S2Etyrec(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2Eexi(x0, xs0) = 
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Euni(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt(), nil_vt()) end
implement{}
simplify_S2Erefarg(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Evararg(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Ewthtype(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2Eerrexp(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end

(* end of datatype s2exp_node *)

(* ****** ****** *)

(* datatype s2rtbas *)  
implement{}
simplify_S2RTBASpre(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2RTBASimp(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2RTBASdef(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end

(* ****** ****** *)

(* datatype s2rt *)

implement{}
simplify_S2RTfun(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end
implement{}
simplify_S2RTtup(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2RTVar(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2RTerr(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in nil_vt() end
implement{}
simplify_S2RTbas(x0, xs0) =
let val () = (free_token(x0); free_toks(xs0)) in (nil_vt(), nil_vt()) end

(* ****** ****** *) // begin Simplify Print

implement{}
simplify_print(xs, color) = (free_toks(xs))

(* ****** ****** *)
*)
