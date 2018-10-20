// Life is fun, ATS is fun so, ATS error reporting should be fun too.

// A wrapper around the ats compiler/typechecker (and its wrappers)
// 
//
// NOTE: This is LARGELY an unfinished product 
//               and is still being actively developed.
//
//
// Primarily seeks to make error reporting easier to read
// with the hope of also creating a unified tool surrounding 
// the ats compiler.
//
// The ideal form of the executable is one tool amasing many tools
// (alike 'cabal' for haskell).
//
// At some point, will merge into this project a REPL-like evaluator 
// for ats that I have been working on as well as a code formatter.
//

#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

//staload "./../SATS/token.sats"

#define TOKENS_NONE

#include "./mylib/bashstr.dats"
#include "./mylib/mylib.dats"

#include "./token.dats"
#include "./tokenize.dats"
#include "./token_lib.dats"
#include "./tok_vt.dats"
#include "./errkind.dats"
#include "./classify_toks.dats"

#include "./print_util.dats"
#include "./simplify_print.dats"
#include "./print_errkind.dats"

#include "./ppatscc.dats"

(* ****** ****** *)


fn print_usage(): void = print!("
acc [whichcc] <filename.dats> 
  
whichcc:
    my         =>    myatscc 
    pc         =>    patscc 
    pm         =>    PATS
    po         =>    patsopt 
    gc         =>    PATS_GC
    tcats      =>    patscc -tcats 
    tc         =>    patscc -tcats 
    lm         =>    PATS_LM
    potc       =>    patsopt --typecheck --dynamic 
    fly        =>    patscc -tcats 
    c          =>    myatscc 
    tcatsc     =>    patscc -tcats 

    myatscc    =>    myatscc 
    patscc     =>    patscc 
    patscc_gc  =>    PATS
    tcats      =>    patscc -tcats 
    patsopt    =>    patsopt 

    where 
    {
      PATS          patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC 
      PATS_GC       patscc -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW 
      PATS_LM       patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -lm
    }

")

(*
  // for future
  --json                        patsopt --jsonize-2 -d 
                                patsopt --jsonize-2 -s 
  --taggen                      patsopt --taggen -s 
                                patsopt --taggen -d
  --patsoptv              
*)


(* 
patsopt --output cf_dats.c --dynamic cf.dats 
*)
(* 
"gcc -std=c99 -D_XOPEN_SOURCE 
  -I${PATSHOME} -I${PATSHOME}/ccomp/runtime -L${PATSHOME}/ccomp/atslib/lib " 
*)
(* 
#define PATS_CLEAN "patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -cleanaft "
*)
(* 
#define 
PATS_LM "patscc -tcats -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -latslib -lm -g " 
*)

#define PATS "patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC "
#define PATS_GC "patscc -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW "
#define PATS_LM "patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC -lm "


fn get_whichcc(name: string): string = 
(
  case+ name of
    | "my" => "myatscc "
    | "pc" => "patscc "
    | "pm" => PATS
    | "po" => "patsopt "
    | "gc" => PATS_GC
    | "tcats" => "patscc -tcats "
    | "tc" => "patscc -tcats "
    | "lm" => PATS_LM
    | "potc" => "patsopt --typecheck --dynamic "
    | "fly" =>  "patscc -tcats "
    | "c" => "myatscc "
    | "tcatsc" => "patscc -tcats "

    | "-my" => "myatscc "
    | "-pc" => "patscc "
    | "-pm" => PATS
    | "-po" => "patsopt "
    | "-gc" => PATS_GC
    | "-tcats" => "patscc -tcats "
    | "-tc" => "patscc -tcats "
    | "-lm" => PATS_LM
    | "-potc" => "patsopt --typecheck --dynamic "
    | "-fly" =>  "patscc -tcats "
    | "-c" => "myatscc "


    | "myatscc" => "myatscc "
    | "patscc" => "patscc "
    | "patscc_gc" => PATS
    | "tcats" => "patscc -tcats "
    | "patsopt" => "patsopt "
    | _ =>  "err" // otherwise error

(*
    | "-my" => "myatscc "
    | "-pc" => "patscc "
    | "-pm" => PATS
    | "-po" => "patsopt "
    | "-gc" => PATS_GC
    | "-tcats" => "patscc -tcats "
    | "-tc" => "patscc -tcats "
    | "-lm" => PATS_LM
    | "-potc" => "patsopt --typecheck --dynamic "
    | "-fly" =>  "patscc -tcats "
    | "-c" => "patscc -tcats "

    | "--myatscc" => "myatscc "
    | "--patscc" => "patscc "
    | "--patscc_gc" => PATS
    | "--tcats" => "patscc -tcats "
    | "--patsopt" => "patsopt "
    | _ =>  "err" // otherwise error
*)

    // for future
    // | "-js" => "atscc2js "
    // | "--atscc2js" => "atscc2js "
)


extern
fun
unlink:(string) -> int = "ext#"

fn
print_c_error
(xs: toks): void = 
(
  println!("\n------------------ C COMPILER MESSAGES ------------------\n");
  print_toks_all(xs);
  println!("\n-------------- END C COMPILER MESSAGES ------------------\n");
)

fn
cleanup
(in_f: FILEref, xy: Strptr1, x: List0_vt(string), fcmd0: Strptr1): void = 
(
    fileref_close(in_f);
    strptr_free(xy);
    free(x);
    strptr_free(fcmd0);
)

fn
is_other_error_type
(listvt_of_tokens: !toks): bool = let
    val first_is0 = head_is_pred
        (listvt_of_tokens, 
          lam i => not(tok_chr_eq(i, '/')) && not(tok_chr_eq(i, '*')))
    val n_is0 = toks_n_is_pred_ide 
        (listvt_of_tokens, 
          lam i => (tok_ide_eq(i, "tmp") || tok_ide_eq(i, "usr")), 0)
in
  first_is0 || n_is0  
end


fn
get_show_options(name: string) : (bool, bool, bool, bool) = let
  //  if second argument true, sets experimental color
  val color_choice = (if (name = "c" || name = "tcatsc") then true else false)
  val linenum = (if name = "fly" || name = "-fly" then false else true)
  val loc = (if name = "fly" || name = "-fly" then false else true)
  val verbose = (if name = "c" || name = "tcatsc" || name = "tc" then false else true)
in
  (color_choice, linenum, loc, verbose)  
end

(*
            arguments  :  creates linear list of strings from user arguments
     list_from_listvt  :  converts from list_vt to list
                          prepends unsafe to manually free linear list later
  stringptr_from_list  :  create one string from the list of strings
string_from_stringptr  :  convert strptr to a string 
                          prepends unsafe to manually free string later
                 name  :  option of whichcc -- if not found, exit
              whichcc  :  get the corresponding option - if not found, exit
    command_stringptr  :  prepends compiler name to args
      command_string   :  complete command to pass to popen
                          prepends unsafe to manually free string later
         logfile_name  :  name of file used to put output of command into
    popen_return_code  :  popen cmd2
           input_file  :  opens file logfile_name 
                          (if empty popen did not write to it.)
   streamvt_of_tokens  :  error messages tokenized
     listvt_of_tokens  :  converts the linear stream to a linear list
*)
fn
run
(arguments: List0_vt(string), name: string): int = let 
    val list_from_listvt = $UN.list_vt2t(arguments)
    val stringptr_from_list = stringlst_concat(list_from_listvt)
    val string_from_stringptr = $UN.strptr2string(stringptr_from_list)
    val whichcc = get_whichcc(name)
    val () = ( if whichcc = "err" 
        then (println!("whichcc error");print_usage(); exit(1) : void))
    val command_stringptr 
      = string_append(whichcc, string_from_stringptr, " 2>&1 ")
    val command_string = $UN.strptr2string(command_stringptr)
    val logfile_name = ".log" : string
    //
    val pipe_return_code = pipe_stream_vt0(command_string, logfile_name)
    val input_file = fileref_open_exn(logfile_name, file_mode_r)
    val stream_of_tokens = tokenize(streamize_fileref_char(input_file))
    val listvt_of_tokens = stream2list_vt(stream_of_tokens)
    //
  val return_code = (  
    ifcase
    | is_other_error_type(listvt_of_tokens)  => 
        let val () = print_c_error(listvt_of_tokens) in 1 end
    | iseqz listvt_of_tokens =>
        let val () = free_toks(listvt_of_tokens) in 0 end
    | (*isneqz xs*)_ => let
        val (color_choice, linenum, loc, verbose) = get_show_options(name)
        //  tokens converted and divided into separate messages
        val messages = tokens_to_messages_free(listvt_of_tokens)
        //  the separated messages are then classified 
        val classified_messages = classify_toks_free(messages)
        //  the classified messages are then pretty-printed
        val () =
        print_classified_free(classified_messages, color_choice, linenum, loc, verbose)
      in 1 end
    )
  val () //  cleanup and exit with non-zero status
    = cleanup(input_file, stringptr_from_list, arguments, command_stringptr)
  val unlink_return_code = unlink(logfile_name);

in
  return_code
end


(* ****** ****** *)


(*
// instead of fix
val x =  ( let
  fun
    loop
    (argc: int n, argv: !argv(n), i: natLte(n), res: List0_vt(string))
    : List0_vt(string) =
        ifcase
        | argc > i => loop(argc, argv, i+1, cons_vt(" ", cons_vt(argv[i], res)))
        | (* else *)_ => list_vt_reverse(res)
      in
          let val () = assertloc(argc >= 2)
          in
            loop(argc, argv, 1, nil_vt())
          end
      end
)
*)


(* ****** ****** *)


implement
main 
{n} (argc, argv) = res where 
{
  val err =   // response to argc = 0 .. 
    (lam () => println!( "... demons may fly out of my nose..."))
  val res = 
    ifcase
    | argc >= 2 =>
    (
      let
      val x = ( 
        fix loop 
        (ac: int n, av: !argv(n), i: natLte(n), res: List0_vt(string))
        : List0_vt(string) =>
            ifcase
            | ac > i => loop(ac, av, i+1, cons_vt(" ", cons_vt(av[i], res)))
            | (*else*)_ => list_vt_reverse(res)

        )(argc, argv, if (argc = 2) then 1 else 2, nil_vt())
      // val () = println!("x = ", x)

      val 
      res = (
        case+ argc of 
        | 2 => (
                 if strstr(g1ofg0(argv[1]), ".dats") >= 0 
                 then run(x, "c") 
                 else (print_usage(); free(x); 1)
               )
        | 3 => let
            val name = argv[1] : string
            val filename = argv[2] : string
          in
            run(x, name)
          end
        | (* >= 4 *)_ => let
            val () = assertloc(argc >= 4)
            val name = argv[1] : string
          in  
            run(x, name)
          end
      )
      in
        res
      end
    )
    | argc = 0 => let val () = (err(); print_usage()) in 9 end 
    | _ => (* argc = 1 => *) let val () = print_usage() in 1 end
}


(* ****** ****** *)

(* end of [main.dats] *)