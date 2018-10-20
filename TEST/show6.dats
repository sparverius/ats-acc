typedef X = '{ a=int, b=string, c=char }

val x = '{ a=0, b="hey", c='c'} : X
//val _ = $showtype x
val _ = $showtype('{ a=0, b="hey", c='c'})//x

val x = '{ a=0, b="hey", c='d' }
//val _ = $showtype x

val x = '{ a=0, b="hey" }
//val _ = $showtype x
val _ = $showtype('{ right=0, left=1 })


val _ = $showtype(list_nil())


val _ = $showtype(lam(x:char) => x)

val _ = $showtype("extern fn foo(x: int, y: int): int")

extern
fn
foo(x: int, y: int): int

val _ = $showtype(foo)

extern
fn
foo(x: int, y: int, b: bool): int

val _ = $showtype(foo)

val () = $showtype("")