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

#define TOKENS_NONE

staload UN = "prelude/SATS/unsafe.sats"

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
  -my     => myatscc 
  -pc     => patscc 
  (* 
  -po     => patsopt  // under development
  *)
  -tc     => patscc -tcats    
  -tcats
  -pcm    => patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC
  -pcgc   => patscc -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW

  --myatscc   => myatscc 
  --patscc    => patscc 
  (*
  --patsopt   => patsopt // under development
  *)
  --patscc_m  => patscc -D_GNU_SOURCE -DATS_MEMALLOC_LIBC
  --patscc_gc => patscc -D_GNU_SOURCE -DATS_MEMALLOC_GCBDW


filename:
  (.sats not yet supported)

example:
  acc -pc foo.dats -o foo 

  error hints: 
     y <~ x  :  'x' should be 'y' 
    y <?~ x  :  'x' should (most likely) be 'y'

          #  :  prefix for external value or external kind 
                (S2Eextype | S2Eextkind) 

        [n]  :  existentially qualified type n (S2Eexi)
        inf  :  intinf (S2Eintinf)
       uni.  :  universally quantified type (S2Euni)

      g0int  :  g0int_t0ype   (unindexed integer)
      g1int  :  g1int_int_t0ype (indexed integer)

      XXXX-
      XXXXX  :  where X is an integer (I believe is a tag of some sort)

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

(*
      x  :  creates linear list of strings from user arguments

      z  :  converts from list_vt to list
            prepends unsafe to manually free linear list later

     xy  :  create one string from the list of strings

    xy1  :  convert strptr to a string 
            prepends unsafe to manually free string later

   name  :  option of whichcc -- if not found, exit

whichcc  :  get the corresponding option - if not found, exit

  fcmd0  :  prepends compiler name to args
            e.g. 
            command = 'acc -tcats foo.dats'
            whichcc = 'patscc
            xy1     = '-tcats foo.dats 
            fcmd0   = 'patscc -tcats foo.dats'

   cmd   :  complete command to pass to popen
            prepends unsafe to manually free string later

logname  :  name of file used to put output of command into

      _  :  popen cmd2

   in_f  :  opens file pipe_stream_vt0 writes output to (logname)

   toks  :  error messages tokenized

     xs  :  converts the linear stream to a linear list for subsequent use

*)

fn
run
(x: List0_vt(string), name: string): int = let
  val z = $UN.list_vt2t(x)
  val xy = stringlst_concat(z)
  val xy1 = $UN.strptr2string(xy)

  val whichcc = get_whichcc(name)
  val () = (if whichcc = "err" then (println!("whichcc error");print_usage(); exit(1) : void) else ())

  val fcmd0 = string_append(whichcc, xy1, " 2>&1 ")
  val cmd = $UN.strptr2string(fcmd0)
  // val () = println!("cmd2 = ", cmd2) // print command string produced
  val logname = ".log" : string
  //
  val _ = pipe_stream_vt0(cmd, logname)
  val in_f = fileref_open_exn(logname, file_mode_r)
  val toks = tokenize(streamize_fileref_char(in_f))
  val xs = stream2list_vt(toks)
  //
  val first_is0 
    = head_is_pred
      (xs, lam i => not(tok_chr_eq(i, '/')) && not(tok_chr_eq(i, '*')))
  val n_is0
    = toks_n_is_pred_ide 
      (xs, lam i => (tok_ide_eq(i, "tmp") || tok_ide_eq(i, "usr")), 0)
  in
    ifcase
    | // defer formatting to gcc output 
       first_is0 || n_is0  => let
        val () = (print_c_error(xs); cleanup(in_f, xy, x, fcmd0))
        val x = unlink(logname);
      in
        (* (exit 0) : void *)
        0
      end
    | iseqz xs => let
        // if 'xs' is empty, cleanup and exit
        val () = (free_toks(xs); cleanup(in_f, xy, x, fcmd0))
        val x = unlink(logname);
      in
        (* (exit 0) : void *)
        0
      end
  | (*isneqz xs*)_ => let
      (*
          xys  :  tokens converted and divided into separate messages 
          zs   :  the separated messages are then classified 
               :  the classified messages are then pretty-printed
               :  last - cleanup and exit with non-zero status
      *)
      val xys = tokens_to_messages_free(xs)
      val zs = classify_toks_free(xys)
      // if second argument true, sets experimental color
(*
      val () = println!("name = ", name)
      val () = println!("whichcc = ", whichcc)
*)

      (* val color_choice = false //(if name = "-fly" then false else true) *)
      val color_choice = (if name = "-c" then true else false)
      val linenum = (if name = "-fly" then false else true)
      val () = print_classified_free(zs, color_choice, linenum) //false)
      val () = cleanup(in_f, xy, x, fcmd0)         
      val x = unlink(logname);
    in
      (* (exit(1)): void // exit :: error *)
      1
    end
  end


implement main {n} (argc, argv) = res
where 
{
  val () = (if argc <= 2 then (print_usage(); exit(1)))
  val x = ( let
    fun
    loop(argc: int n, argv: !argv(n), i: natLte(n), res: List0_vt(string))
    : List0_vt(string) 
      =
        if argc > i then let
            val res2 = cons_vt(argv[i], res)
            val res2 = cons_vt(" ", res2)
          in
            loop(argc, argv, i+1, res2)
          end
        else list_vt_reverse(res)
      in
          let val () = assertloc(argc >= 2)
          in
            loop(argc, argv, 2, nil_vt())
          end
      end
  )
  val name = (if argc >= 2 then argv[1] else "~1"): string
  val () = (if name = "~1" then (println!("argc < 2"); print_usage(); exit(1) : void) else ())

  val filename = (if argc >= 3 then argv[2] else "~1"): string
  val () = (if name = "~1" then (println!("argc < 3"); print_usage(); exit(1) : void) else ())

  (* val () = println!("filename = ", filename, "\n\n") *)
  val res = run(x, name)
}

(* ****** ****** *)

(* end of [main.dats] *)