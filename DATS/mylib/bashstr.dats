(* ****** ****** *)

(*
#include 
"share/atspre_staload.hats"
#include 
"share\
/atspre_staload_libats_ML.hats"
*)

(* ****** ****** *)

#define BASHSTR

#define reset_all "0" 

#define bold "1"
#define dim "2"
#define underline "4"
#define blink "5"
#define reverse "7"
#define hidden "8"

#define reset_bold  "21"
#define reset_bright "22"
#define reset_dim "24"
#define reset_blink "25"
#define reset_reverse "27"
#define reset_hidden "28"

#define default_foreground "39"
#define black "30"
#define red "31"
#define green "32"
#define yellow "33"
#define blue "34"
#define magenta "35"
#define cyan "36"
#define light_gray "37"

#define dark_gray "90"
#define light_red "91"
#define light_green "92"
#define light_yellow "93"
#define light_blue "94"
#define light_magenta "95"
#define light_cyan "96"
#define white "97"

#define default_background "49"
#define black_background "40"
#define red_background "41"
#define green_background "42"
#define yellow_background "43"
#define blue_background "44"
#define magenta_background "45"
#define cyan_background "46"
#define light_gray_background "47"

#define dark_gray_background "100"
#define light_red_background "101"
#define light_green_background "102"
#define light_yellow_background "103"
#define light_blue_background "104"
#define light_magenta_background "105"
#define light_cyan_background "106"
#define white_background "107"


extern
fn 
print_color
(num:string): void


implement
print_color(num) =
(print("\033["); print(num); print("m"))

fn print_color_close(): void = print_color(reset_all) // ("\033[0m")

#define prc(i) print_color(i)
#define prcc print_color_close()

extern
fun
(* {a:t@ype} *)
print_esc(num: string, xa: string): void

implement
(* {a} *)
print_esc (num, xa): void = (prc num ; print(xa) ; prcc)


extern
fn print_esc_nl (num: string, xa: string): void

implement
(* {a} *)
print_esc_nl (num, xa): void 
  = (prc num ; print(xa) ; prcc ; print "\n")

overload olor with print_esc
overload olorln with print_esc_nl

fn
print_a_color
(colorname: string) = let
  val name = (
    case+ colorname of
    | "reset_all" => reset_all
    | "reset_dim" => reset_dim
    | "default" => default_foreground
    | "bold" => bold
    | "dim" => dim
    | "underline" => underline
    | "blink" => blink
    | "reverse" => reverse
    | "hidden" => hidden
    | "red" => red
    | "cyan" => cyan
    | "blue" => blue
    | "green" => green
    | "black" => black
    | "white" => white
    | "yellow" => yellow
    | "magenta" => magenta
    | "dark_gray" => dark_gray
    | "light_red" => light_red
    | "light_blue" => light_blue
    | "light_cyan" => light_cyan
    | "light_gray" => light_gray
    | "light_green" => light_green
    | "light_yellow" => light_yellow
    | "light_magenta" => light_magenta
    | _ => reset_all
  )
in
  prc(name)
end


//(print_color(num); print(xa); print_color(reset_all))

(* ****** ****** *)

////
implement main0 () = () where
{
// // TESTING
// // val () = print_esc(light_blue_background, "HELLO THERE")
val colors = 
g0ofg1($list{int}
(1,2,4,5,7,8,30,31,32,33,34,35,36,37,90,91
,92,93,94,95,96,97,40,41,42,43,44,45,46,47
,100,101,102,103,104,105,106,107))

#define foreach list0_foreach

val () = foreach(colors, lam x => (print_esc(tostring_int(x), "HELLO"); print!("  ")))
val () = println!()
}
(* ****** ****** *)

(* end of [bashstr.dats] *)