#include "share/atspre_staload.hats"

(* typedef X = @{ a= int, b=string } *)
(* typedef X = $tup(int, string) *)
typedef X1 = '{ a=int, b=string }
//val x = cons_vt('c', nil_vt()) : List0_vt(X1)
//val _ = $showtype x1


datavtype X1 = 
  | X1int0 of int

val x = X1int0(10)
val _ = $showtype(x)

datatype X0 =
  | int0 of int
  | {n:int} int1 of int(n)
  | intu of uint

val x = int0(10)
val _ = $showtype x

val x = int1(10)
val _ = $showtype x

val x = int1(10)
val-int1(x) = x
val _ = $showtype x

val x = intu(10u)
val-intu(x) = x
val _ = $showtype x


typedef X = '{ a=int, b=string, c=string, d=int, e=int }
val x = '{ a=0, b="hey", c="there", d=1, e=2 } : X
val _ = $showtype x

val x = 1
val _ = $showtype x
val x = g0ofg1(1)
val _ = $showtype x

val x = g0ofg1(1)
val _ = $showtype x



val x = '{ a=0, b="hey", c="there", d=1, e=2 }
val _ = $showtype x
val x = @{ a=0, b=g0ofg1("hey"), c=g0ofg1("there"), d=g0ofg1(1), e=2 }
val _ = $showtype x





implement main0() = ()