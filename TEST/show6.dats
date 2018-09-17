#include "share/atspre_staload.hats"


typedef X = '{ a=int, b=string, c=char }

val x = '{ a=0, b="hey", c='c'} : X
val _ = $showtype 
x
val x = '{ a=0, b="hey", c='d' }
val _ = $showtype x
val x = '{ a=0, b="hey" }
val _ = $showtype x





implement main0() = ()