
(* staload "libats/libc/SATS/stdio.sats" *)

(* fn  *)
(* pipe_stream_vt0 *)
(* (cmd: string (\* , cs0: FILEref *\)): int  *)
(*   = status where  *)
(* { *)
(*   val rfp = popen_exn(cmd, $UN.cast{pmode(r)}("r")) *)
(*   val cs0 = fileref_open_exn("tmp", file_mode_ww) *)
(*   val ()  *)
(*     = while (true) *)
(*       { *)
(*         val str = fgets0_gc (2, rfp) *)
(*         val () = assertloc (strptr2ptr (str) > 0) *)
(*         val isemp = strptr_is_empty (str) *)
(*         val () = ( *)
(*           ifcase *)
(*           | isemp => (strptr_free(str); $break) *)
(*           | _ =>  *)
(*             (fputs_exn($UN.strptr2string(str), cs0); strptr_free (str)) *)
(*         ) *)
(*       } *)
(*   val status = pclose0_exn(rfp) *)
(*   val () = fileref_close(cs0) *)
(* } *)




staload UN = "prelude/SATS/unsafe.sats"

typedef cfun(a: t@ype, b: t@ype) = a -<cloref1> b

extern
fun
{a:t@ype}
list_skip_while
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)

extern
fun
{a:t@ype}
list_skip_until
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)

extern
fun
{a:t@ype}
list_skip_until_in
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)

extern
fun
{a:t@ype}
list_take_while
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)

extern
fun
{a:t@ype}
list_take_until
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a) //, List0_vt(a))

extern
fun
{a:t@ype}
list_take_until_in
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)


extern
fun
{a:t@ype}
list_get_head_tail
(xs: List0(INV(a))): (Option(a), List0(a))


fun
{a:t@ype}
list_vt2t_rev(xs: List0_vt(a)): (List0(a), List0_vt(a)) = //  list_vt2t(list_vt_reverse(xs))
(
  let
    val x = list_vt_reverse(xs)
    val y = $UN.list_vt2t(x)
  in
    (y, x)
  end
)


overload lvt2tr with list_vt2t_rev


fun
{a:t@ype}
listrev(xs: List0(a)): List0(a) =
let
fun aux(xs: List0(a), res: List0(a)): List0(a) =
case+ xs of
| nil() => res
| cons(x, xs) => aux(xs, cons(x, res))
in
  aux(xs, nil())
end

implement
{a}
list_skip_while
  (xs, pred) = auxmain(xs) where
{
  fun
  auxmain
  (xs: List0(a)): List0(a) =
  (
    case+ xs of
    | nil() => nil()
    | cons(x0, xs1) => 
      if pred(x0) then auxmain(xs1) else xs
  )
}


implement
{a}
list_skip_until
  (xs, pred) = auxmain(xs) where
{
  fun
  auxmain
  (xs: List0(a)): List0(a) =
  (
    case+ xs of
    | nil() => nil()
    | cons(x0, xs1) => 
      if pred(x0) then xs else auxmain(xs1)
  )
}


implement
{a}
list_skip_until_in
  (xs, pred) = auxmain(xs) where
{
  fun
  auxmain
  (xs: List0(a)): List0(a) =
  (
    case+ xs of
    | nil() => nil()
    | cons(x0, xs1) => 
      if pred(x0) then xs1 else auxmain(xs1)
  )
}



implement
{a}
list_take_while
  (xs, pred) = let
fun
auxmain
(xs: List0(a), res: List0_vt(a)): List0_vt(a) =
(
  case+ xs of
  | nil() => res
  | cons(x0, xs1) => 
    if pred(x0) 
      then auxmain(xs1, list_vt_cons(x0, res)) else res
)
in
  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
end


implement
{a}
list_take_until
  (xs, pred) = let
fun
auxmain
(* (xs: List0(a), res: List0(a)): List0(a) = *)
(* ( *)
(*   case+ xs of *)
(*   | nil() => res *)
(*   | cons(x0, xs1) =>  *)
(*     if pred(x0) then res else auxmain(xs1, list_cons(x0, res)) *)
(* ) *)
(* in *)
(*   listrev(auxmain(xs, list_nil())) *)
(* end *)
(xs: List0(a), res: List0_vt(a)): List0_vt(a) =
(
  case+ xs of
  | nil() => res
  | cons(x0, xs1) =>
    if pred(x0)
      then res else auxmain(xs1, list_vt_cons(x0, res))
)
in
  (* let *)
  (*   val (x,y) = list_vt2t_rev(auxmain(xs, list_vt_nil())) *)
  (*   (\* val y = $UN.list_vt2t(x) *\) *)
  (* in *)
  (*   (x, y) *)
  (* end *)
 list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
end



implement
{a}
list_take_until_in
  (xs, pred) = let
fun
auxmain
(xs: List0(a), res: List0_vt(a)): List0_vt(a) =
(
  case+ xs of
  | nil() => res
  | cons(x0, xs1) => 
    let 
      val res = list_vt_cons(x0, res)
    in
      if pred(x0) then res 
      else auxmain(xs1, res)
    end
)
in
  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
end



extern
fun
{res: t@ype}
{a:t@ype}
list_foldleft0
(xs: List0(a), init: res, fopr: (res, a) -<cloref1> res): res

implement
{res}{a}
list_foldleft0
  (xs, init, fopr) = loop(init, xs) where
{
  fun
  loop(res: res, xs: List0(a)): res =
  (
    case+ xs of
    | list_nil() => res
    | list_cons(x, xs) => loop(fopr(res, x), xs)
  )
}

(*
let
//  val ys = list_filter<token>(xs)
in
  list_foldleft(xs)
end
*)



implement
{a}
list_skip_until
  (xs, pred) = auxmain(xs) where
{
  fun
  auxmain
  (xs: List0(a)): List0(a) =
  (
    case+ xs of
    | nil() => nil()
    | cons(x0, xs1) => 
      if pred(x0) then xs else auxmain(xs1)
  )
}


(* #define lvt2tr( ls ) list_vt2t(list_vt_reverse( ls )) *)


fun
{a:t@ype}
list_takeskip_until
(xs: List0(a), pred: cfun(a, bool)): (List0(a), List0(a), List0_vt(a)) = let
fun
auxmain
(xs: List0(a), res: List0_vt(a)): (List0_vt(a), List0(a)) =
(
  case+ xs of
  | nil() => (res, xs) // not so sure about that
  | cons(x0, xs1) => 
    if pred(x0) then (res, xs) else auxmain(xs1, list_vt_cons(x0, res))
)
in
  let
    val xs0 = auxmain(xs, list_vt_nil())
    val xs00 = xs0.0
//
    val (x0, freeme) = list_vt2t_rev(xs00)
  in
    (x0, xs0.1, freeme)  
//    ((lvt2tr(xs00)) : List0(a), xs0.1 )
  //  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
  end

end

fun
{a:t@ype}
list_takeskip_until_in
(xs: List0(a), pred: cfun(a, bool)): (List0(a), List0(a), List0_vt(a)) = let
fun
auxmain
(xs: List0(a), res: List0_vt(a)): (List0_vt(a), List0(a)) =
(
  case+ xs of
  | nil() => (res, xs) // not so sure about that
  | cons(x0, xs1) => 
    let
      val res = list_vt_cons(x0, res)
    in
      if pred(x0) then (res, xs1) else auxmain(xs1, res) //list_vt_cons(x0, res))
    end
)
in
  let
    val xs0 = auxmain(xs, list_vt_nil())
    val xs00 = xs0.0
//    val x0 = (lvt2tr(xs00)) : List0(a)
    val (x0, freeme) = list_vt2t_rev(xs00)
  in
    (x0, xs0.1, freeme)  
//    ((lvt2tr(xs00)) : List0(a), xs0.1 )
  //  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
  end

end


(* 
// venture only 2 values into list
// if list non nil then return true
fun
{a:t@ype}
list_length_drop_head
(xs: List0(INV(a))): bool =
*)



implement
{a}
list_get_head_tail
(xs: List0(INV(a))): (Option(a), List0(a)) =
let
  val head = (if isneqz xs then Some(list_head_exn(xs)) else None()) : Option(a)
  val rest = (if length xs >= 2 then list_drop_exn(xs, 1) else xs)
in
  (head, rest)
end



extern
fun
{a:t@ype}
list_head_opt
(xs: List0(INV(a))): Option(a)

implement
{a}
list_head_opt(xs) = 
(if isneqz xs then Some(list_head_exn(xs)) else None()) : Option(a)



extern
fun
{a:t@ype}
list_skip_until_second // second occurence
(xs: List0(INV(a)), pred: cfun(a, bool)): List0(a)


implement
{a}
list_skip_until_second
  (xs, pred) = auxmain(xs, false) where
{
  fun
  auxmain
  (xs: List0(a), check: bool): List0(a) =
  (
    case+ xs of
    | nil() => nil()
    | cons(x0, xs1) => 
      if pred(x0) && check then xs 
      else if pred(x0) && not(check) then auxmain(xs1, true)
      else auxmain(xs1, false)
  )
}





extern
fun
{a:t@ype}
list_take_until_n
(xs: List0(INV(a)), n: int, pred: cfun(a, bool)): List0(a)

implement
{a}
list_take_until_n
  (xs, n, pred) = let
fun
auxmain
(xs: List0(a), res: List0_vt(a), i: int): List0_vt(a) =
(
  case+ xs of
  | nil() => res
  | cons(x0, xs1) => 
    if pred(x0) && n - i = 0 then res
    else if pred(x0) then auxmain(xs1, list_vt_cons(x0, res), i+1)
    else auxmain(xs1, list_vt_cons(x0, res), i)
      (* then res *)
      (* else auxmain(xs1, list_vt_cons(x0, res)) *)
)
in
  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil(), 0)))
end



fun
{a:t@ype}
list_takeskip_until_n
(xs: List0(a), n: int, pred: cfun(a, bool)): (List0(a), List0(a), List0_vt(a)) = let
//val n = (if n >= 1 then n else 1) : int
fun
auxmain
(xs: List0(a), i: int, res: List0_vt(a)): (List0_vt(a), List0(a)) =
(
  case+ xs of
  | nil() => (res, xs) // not so sure about that
  | cons(x0, xs1) => 
    if pred(x0) && n - i = 0 then (res, xs)
    else if pred(x0) then auxmain(xs1, i+1, list_vt_cons(x0, res))
    else auxmain(xs1, i, list_vt_cons(x0, res))
    (* if pred(x0) then (res, xs) else auxmain(xs1, list_vt_cons(x0, res)) *)
)
in
  let
    val xs0 = auxmain(xs, 0, list_vt_nil())
    val xs00 = xs0.0
    val (x0, freeme) = list_vt2t_rev(xs00)
  in
    (x0, xs0.1, freeme)  
  //  list_vt2t(list_vt_reverse(auxmain(xs, list_vt_nil())))
  end
end


(* fn *)
(* {a:t@ype}  *)
(* reverse *)
(* {n:nat} *)
(* (xs: list_vt (a, n)) : list_vt (a, n) = let *)
(*   fun  *)
(*   revapp *)
(*   {i,j:nat | i+j==n} .<i>. *)
(*   (xs: list_vt (a, i), ys: list_vt (a, j)) : list_vt (a, n) = *)
(*     case+ xs of *)
(*     | @list_vt_cons(_, xs1) => let *)
(*         val xs1_ = xs1 *)
(*         val () = xs1 := ys *)
(*         prval () = fold@ (xs) *)
(*       in *)
(*         revapp (xs1_, xs) *)
(*       end *)
(*     | ~list_vt_nil () => ys *)

(* in *)
(*   revapp (xs, list_vt_nil) *)
(* end // end of [reverse] *)

(* fn *)
(* {a:t@ype}  *)
(* reverse *)
(* {n:nat} *)
(* (xs: list_vt(a, n)) : list_vt (a, n) = let *)
(*   fun  *)
(*   revapp *)
(*   {i,j:nat | i+j==n} .<i>. *)
(*   (xs: list_vt (a, i), ys: list_vt (a, j)) : list_vt (a, n) = *)
(*     case+ xs of *)
(*     | @list_vt_cons(_, xs1) => let *)
(*         val xs1_ = xs1 *)
(*         val () = xs1 := ys *)
(*         prval () = fold@ (xs) *)
(*       in *)
(*         revapp (xs1_, xs) *)
(*       end *)
(*     | ~list_vt_nil () => ys *)

(* in *)
(*   revapp (xs, list_vt_nil) *)
(* end // end of [reverse] *)

fun
{a:t@ype}
list_vt_make_List0
{n:nat} .<n>.
(xs: list_vt (a, n)): List0(a) = let
fun
aux
{n:nat} .<n>.
(xs: list_vt(a, n), res: List0(a)): List0(a) = 
case+ xs of
| @list_vt_cons(x, xs1) => let
    val xs1_ = xs1
    val res = list_cons(x, res)
    val () = free@{a}{0}(xs) 
  in 
    aux(xs1_, res)
  end
| @list_vt_nil() => 
  let val () = free@{a} (xs) in res end
in
  aux(xs, list_nil())
end  
  
