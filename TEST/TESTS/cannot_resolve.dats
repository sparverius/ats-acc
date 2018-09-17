#include "share/atspre_staload.hats"

implement main0() = ()


typedef row (a:t@ype) = @[a][3]

typedef arow (t) = @[t][3]

typedef mat2 (a:t@ype) = @(row(a), row(a))

var row1 = @[int](1,2,3) : row(int)
var row2 = @[int](4,5,6) : arow(int)


fn
get_row(x: int, irow: int, a: &(row(int)), b: &(row(int))): void = 
  if x = 0 then let (* val x0 = a[irow]  *)in print(a[irow]) end
     else let val x1 = b.irow in print(x1) end

