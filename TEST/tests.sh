#!/bin/bash

files=( "./TEST/cfail.dats"
	"./TEST/atsgu1.dats"
	"./TEST/fail2.dats"
	"./TEST/show6.dats"
      )

code0="datatype FIB(int, int) =
| FIB0(0,0) of ()
| FIB1(1,1) of ()
| {n: int | n >= 2}{r1,r2:int}
  FIB2(n, r1+r2) of (FIB(n-1, r1), FIB(n-2, r2))

extern
fun
fib{n:nat}
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
      //                             ^ error here\n  
    end
"
code1="var a:int = 0
var b:int = 1
val () = ( a :=: b )
"
code2="val 42 = 24 : uint
"
code3="typedef X = '{ a=int, b=string, c=char }

val x = '{ a=0, b=\"hey\", c='c'} : X
val _ = \$$showtype x

val x = '{ a=0, b=\"hey\", c='d' }
val _ = \$$showtype x

val x = '{ a=0, b=\"hey\" }
val _ = \$$showtype x
"


code=( "$code0" "$code1" "$code2" "$code3" )

c=0
for (( c=0; c<=3; c++ ))
do  
    echo -e "\e[1;32m*************** TEST $c ***************\e[0m"
    echo
    echo -e "\e[31m${code[$c]}\e[0m"
    echo -e "\e[1;95mpatscc\e[0m"
    echo -e "\e[95mpatscc -tcats ${files[$c]}...\e[0m"
    echo
    patscc -tcats "${files[$c]}"
    echo
    echo -e "\e[1;94macc\e[0m"
    echo -e "\e[94m./acc -tcats ${files[$c]}...\e[0m"
    ./acc -tcats "${files[$c]}"
    echo
    echo -e "\e[31m*********** end TEST $c ***************\e[0m\n"
done


errs="./TEST/failure.dats"
echo -e "\e[1;32m*************** TEST 4 ***************\e[0m\n"
echo -e "\e[31mtest for memory leaks\n\e[0m"
echo -e "\e[1;95mpatscc\e[0m"
echo -e "\e[95mvalgrind patscc -tcats $errs...\e[0m\n"
valgrind patscc -tcats "$errs" 2>&1 | grep --color=none "^=="
echo -e "\n"
echo -e "\e[1;94macc\e[0m"
echo -e "\e[94mvalgrind ./acc -tcats $errs...\e[0m\n"
valgrind ./acc -tcats "$errs" | grep --color=none "^=="
echo 
echo -e "\e[31m*********** end TEST 4 ***************\e[0m"

echo "
obviously if 'patscc' has memory leaks and 'acc' calls patscc internally then
hidden leaks exist... however, acc itself does not exhibit memory leaking (and
with no perceived GC)...

must now work on significantly decreasing the amount of memory allocation
"


echo -e "\e[31m*********** end tests.sh *************\e[0m"
