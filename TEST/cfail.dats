(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
(* #include *)
(* "share\ *)
(* /atspre_staload_libats_ML.hats" *)
//
(* ****** ****** *)

datatype FIB(int, int) =
| FIB0(0,0) of () 
| FIB1(1,1) of () 
| {n: int | n >= 2}{r1,r2:int}
  FIB2(n, r1+r2) of (FIB(n-1, r1), FIB(n-2, r2))

// {n:int} for all 'quantifier'
// [r:int] existential ... for all n exists r

extern
fun
fib
{n:nat}
(x: int(n)): [r:int] (FIB(n, r) | int(r))

implement
fib(x) =
ifcase
| x = 0 => (FIB0() | 0)
| x = 1 => (FIB1() | 1)
| _ (*else*) => let
  val res1 = fib(x-1)
  val res2 = fib(x-2)
  in
    (FIB2(res1.0, res2.0) | res1.1 - res2.1)
  end

(* val fib3 = fib(~3) *)
(* val () = println!("Value: fib(3) = ", fib3.1) *)

(* ****** ****** *)

implement main0() = () 