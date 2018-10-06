#ifndef TOKENS_NONE

  #include "./tokenize.dats"
  #include "./mylib/bashstr.dats"
  #include "./token_lib.dats"
  #include "./tok_vt.dats"
  #include "./errkind.dats"
  #include "./classify_toks.dats"

#endif

(* ****** ****** *)

(*
#define print_space print(" ")
#define print_ident3 print("   ")
#define print_ident6 print("      ")
//#define print_ident3_nl print("\n   ")
#define print_ident3_nl print("   ")
#define print_ident6_nl print("\n      ")

//#define print_after_error3 print("\n      ")
#define print_after_error3 print(" ")
*)

#define print_space print(" ")
#define print_ident3 print("   ")
#define print_ident6 print("      ")
//#define print_ident3_nl print("\n   ")
#define print_ident3_nl print("   ")
#define print_ident6_nl print("\n      ")



//#define print_after_error3 print("\n      ")
#define print_after_error3 print("")
(* #define print_after_message print("\n          ") //print("\n") *)

#define print_after_all print("\n")
//print("\n\n")

(* ****** ****** *)

#define nl print("\n")
#define nl2 print("\n\n")

(* ****** ****** *)

fn
print_after_message(str: string): void = 
(
  (* nl; *)
  print("\n          "); 
  print(str)
)

#define print_after print("\n          ")


fn 
show_errkind(xs: !errkind): void =
(
  print_ident3;
  print ">>> ";
  print(get_errkind_string(xs));
  print " <<<\n";
)


fn
print_toks_color
(xs: toks, color: bool, colorname: string): void = 
  (
    ifcase
    | color => (
        print_a_color(colorname); 
        print_toks_free_nonewln(xs); 
        prcc
      ) 
    | _ => print_toks_free_nonewln(xs)
  )


fn
print_toks_color_err
(xs: toks, color: bool): void = print_toks_color(xs, color, "red")

fn
print_str_colors
(xs: string, color: bool, color1: string, color2: string): void = 
  (
    ifcase
    | color => (
        print_a_color(color1);
        print_a_color(color2);
        print(xs);
        prcc
      )
    | _ => print xs
  )

fn
print_str_color
(xs: string, color: bool, colorname: string): void = 
  (
    ifcase
    | color => (print_a_color(colorname); print(xs); prcc)
    | _ => print xs
  )


fn
print_str_color_err_dimm
(xs: string, color: bool): void = 
  print_str_colors(xs, color, "red", "dimm")
fn
print_str_color_err
(xs: string, color: bool): void =   (* print_str_color_err_dimm(xs, color) *)
  print_str_color(xs, color, "red")
fn 
print_str_color_sgn
(xs: string, color: bool): void = 
  print_str_colors(xs, color, "light_blue", "bold")
fn
print_str_color_show
(xs: string, color: bool): void = 
  print_str_color(xs, color, "light_cyan")



(* ****** ****** *)

(* end of [print_util.dats] *)