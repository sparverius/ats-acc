#!/bin/bash

files=( "./TEST/cfail.dats"
	"./TEST/atsgu1.dats"
	# "./TEST/fail2.dats"
	"./TEST/list0_tests.dats"
	"./TEST/show6.dats"
      )

ESC_CODE="\e["
ESC_RESET="\e[0m"
BOLD="1"
RED="31"
GREEN="32"
YELLOW="33"
BLUE="34"
MAGENTA="35"
CYAN="36"
LIGHTRED="91"
LIGHTGREEN="92"
LIGHTYELLOW="93"
LIGHTBLUE="94"
LIGHTMAGENTA="95"
LIGHTCYAN="97"
RESET_BOLD="21"


print_color_close() {
    printf "$ESC_RESET"
}

print_color() {
    # takes 1 argument -- color
    printf "$ESC_CODE$1m"
}

print_color_with_string() {
    # takes 2 arguments -- color , string
    printf "$ESC_CODE$1m $2$ESC_RESET"
    # print_color_close
}

print_two_colors() {
    # takes two escape codes
    printf "$ESC_CODE$1;$2m"
}

print_two_colors_with_string() {
    # takes two escape codes
    printf "$ESC_CODE$1;$2m$3"
    print_color_close
}

print_nl2() {
    printf "\n\n"
}

print_test() {
    # takes 1 argument -- test number
    print_color "$GREEN"
    printf "*************** TEST $1 ***************"
    print_color_close
    echo
    # echo -e "\e[1;32m*************** TEST $1 ***************\e[0m"
}

print_test_end() {
    # takes 1 argument -- test number
    print_color "$RED"
    printf "*********** end TEST $1 ***************"
    print_color_close
    print_nl2
}

println() {
    # 1 argument
    [ -z "$1" ] && echo || printf "$1\n"
}

compile_code_with_patscc() {
    echo
    print_two_colors "$BOLD" "$LIGHTMAGENTA"
    printf "patscc"
    print_color_close
    println
    print_color "$LIGHTMAGENTA"
    printf "patscc -tcats $1 ..." # ${files[$c]}..."
    print_color_close
    echo
    patscc -tcats "$1"
}


compile_code_with_acc() {
    print_two_colors "$BOLD" "$LIGHTBLUE"
    printf "acc"
    echo
    print_color_close
    print_color "$LIGHTBLUE"
    printf "./acc -tcats $1 ..."
    print_color_close

    ./acc tcatsc "${files[$c]}"

    echo
}


get_status()
{
    if [ -z "$1" ]; then
        echo "command not given";
    else
        which "$1" &>/dev/null ; echo "$?"
    fi
}

valgrind_message="
obviously if 'patscc' has memory leaks and 'acc' calls patscc internally then
hidden leaks exist... however, acc itself does not exhibit memory leaking (and
with no perceived GC)...

must now work on significantly decreasing the amount of memory allocation
"
valgrind_message_patscc="
==20540== Memcheck, a memory error detector
==20540== Copyright (C) 2002-2017, and GNU GPL'd, by Julian Seward et al.
==20540== Using Valgrind-3.13.0 and LibVEX; rerun with -h for copyright info
==20540== Command: patscc -tcats ./TEST/failure.dats
==20540== 
==20540== 
==20540== HEAP SUMMARY:
==20540==     in use at exit: 95 bytes in 6 blocks
==20540==   total heap usage: 12 allocs, 6 frees, 1,199 bytes allocated
==20540== 
==20540== LEAK SUMMARY:
==20540==    definitely lost: 15 bytes in 1 blocks
==20540==    indirectly lost: 0 bytes in 0 blocks
==20540==      possibly lost: 0 bytes in 0 blocks
==20540==    still reachable: 80 bytes in 5 blocks
==20540==         suppressed: 0 bytes in 0 blocks
==20540== Rerun with --leak-check=full to see details of leaked memory
==20540== 
==20540== For counts of detected and suppressed errors, rerun with: -v
==20540== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
"

valgrind_message_acc="
==20543== Memcheck, a memory error detector
==20543== Copyright (C) 2002-2017, and GNU GPL'd, by Julian Seward et al.
==20543== Using Valgrind-3.13.0 and LibVEX; rerun with -h for copyright info
==20543== Command: ./acc -tcats ./TEST/failure.dats
==20543== 
==20543== 
==20543== HEAP SUMMARY:
==20543==     in use at exit: 0 bytes in 0 blocks
==20543==   total heap usage: 1,140 allocs, 1,140 frees, 35,594 bytes allocated
==20543== 
==20543== All heap blocks were freed -- no leaks are possible
==20543== 
==20543== For counts of detected and suppressed errors, rerun with: -v
==20543== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
"

show_valgrind_test() {
    errs="./TEST/failure.dats"
    print_test "4"
    echo
    print_color_with_string "$RED" "test for memory leaks using valgrind"
    echo

    if [ "$(get_status "valgrind")" -eq "0" ]; then
	# if valgrind command exists
	
	print_two_colors "$BOLD" "$LIGHTMAGENTA"
	printf "patscc"
	print_color_close
	echo
	print_color "$LIGHTMAGENTA"
	printf "valgrind patscc -tcats $errs ..."
	print_color_close
	echo

	valgrind patscc -tcats "$errs" 2>&1 | grep --color=none "^=="

	echo
	print_two_colors "$BOLD" "$LIGHTBLUE"
	printf "acc"
	print_color_close
	echo
	print_color "$LIGHTBLUE"
	printf "valgrind acc -tcats $errs..."
	print_color_close
	echo

	valgrind ./acc -tcats "$errs" | grep --color=none "^=="

	echo 
    else
	echo "valgrind is not installed"
	echo "the output would be similar to below"
	print_color "$LIGHTMAGENTA"
	printf "patscc"
	echo
	printf "valgrind patscc -tcats ./TEST/failure.dats ..."
	print_color_close

	echo "$valgrind_message_patscc"

	print_color "$LIGHTBLUE"
	printf "acc"
	echo
	printf "valgrind acc -tcats ./TEST/failure.dats..."
	print_color_close

	echo "$valgrind_message_acc"
    fi
    print_test_end "4"
    echo "$valgrind_message"    
}



print_end_all_test() {
    print_color "$RED"
    printf "******************* end tests.sh *******************"
    print_color_close
    echo
}


show_patscc_vs_acc_tests() {
    c=0
    for (( c=0; c<=3; c++ ))
    do  
	print_test "$c"
	echo
	# echo "File: ${files[$c]}"
	# source_code_from_file=$(<"${files[$c]}")
	# print_color_with_string "$RED" "$source_code_from_file"
	# echo
	compile_code_with_patscc "${files[$c]}"
	echo
	compile_code_with_acc "${files[$c]}"
    done
}

main() {
    show_valgrind_test
    echo
    show_patscc_vs_acc_tests
    echo
    print_end_all_test
}

main
