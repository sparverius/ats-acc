#ifndef TOKENS_NONE

#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
#include "./bashstr.dats"

#endif

(* ****** ****** *)

staload "libats/libc/SATS/stdio.sats"

(*
extern
fun
popen_exn:(string, string) -> FILEref = "mac#popen"

extern
fun
fputs_exn : (string, FILEref) -> void = "mac#fputs"

extern
fun
fgets0_gc : (int, FILEref) -> Strptr0 = "mac#fgets"

extern
fun
pclose0_exn : (FILEref) -> int = "mac#pclose"
*)

extern
fn 
pipe_stream_vt0 // version used
(cmd: string, logname: string): int 

implement
pipe_stream_vt0(cmd, logname)
  = status where 
{
  val rfp = popen_exn(cmd, $UN.cast{pmode(r)}("r"))
  val cs0 = fileref_open_exn(logname, file_mode_ww)
  val () 
    = while (true)
      {
        val str = fgets0_gc (2, rfp)
        val () = assertloc (strptr2ptr (str) > 0)
        val isemp = strptr_is_empty (str)
        val () = (
          ifcase
          | isemp => (strptr_free(str); $break)
          | _ => 
            (fputs_exn($UN.strptr2string(str), cs0); strptr_free (str))
        )
      }
  val status = pclose0_exn(rfp)
  val () = fileref_close(cs0)
}

(* ****** ****** *)

(*
staload STDLIB = "libats/libc/SATS/stdlib.sats"

extern
fun
do_system
(str: string): void

implement
do_system
  (str) =
let
 val x =  $STDLIB.system(str)
in
  if x != 0
    then println!("error: 'system(", str, ") failed")
end

fn
pipe_stream_vt_system(cmd: string, file_in: FILEref): stream_vt(char) = xs where
{
  val cmd2 = string_append(cmd, " 2>&1 | cat > .log")
  val cmd3 = $UN.strptr2string(cmd2)
  val () = println!("command = ", cmd3)
  val xs0 = do_system(cmd3)
  val () = strptr_free(cmd2)
  val xs  = streamize_fileref_char(file_in)
}
*)


(* ****** ****** *)

(* end of [mylib.dats] *)














fun
pipe_stream_vt1
  (cmd: string): stream_vt(charNZ) = svt where 
{
val rfp = popen_exn(cmd, $UN.cast{pmode(r)}("r"))

val svt =
(
let
fun 
loop(res: stream_vt(charNZ)): stream_vt(charNZ) =
  let
    val str = fgets0_gc(2, rfp)
    val () = assertloc (strptr2ptr(str) > 0)
    val isempty = strptr_is_empty(str)
  in
    ifcase
    | not(isempty) => let
       val y = streamize_string_char($UN.strptr2string(str))
       val () = strptr_free(str)
       val x = stream_vt_append(res, loop(y)) : stream_vt(charNZ)
      in 
        x
      end
    | _ => let
        val () = strptr_free(str)
       in 
         res
       end
  end
in
  loop(stream_vt_make_nil())
end
): stream_vt(charNZ)

val status = pclose0_exn(rfp)
}


extern
fun{}
streamize_strptr_char(str: Strptr): stream_vt(charNZ)

  typedef elt = charNZ

implement
{}(*tmp*)
streamize_strptr_char(str): stream_vt(charNZ) = let
  fun
  auxmain (p: ptr) : stream_vt(elt) 
    = $ldelay
    (
      let
        val c0 = $UN.ptr0_get<Char>(p)
        (* val tmp = tostring_char(c0) *)
        (* val () = olor(cyan, tmp) *)
      in
        ifcase
        | isneqz(c0) => stream_vt_cons(c0, auxmain(ptr0_succ<Char>(p))) //, s))
        | _ => let 
          (* val () = strptr_free($UN.castvwtp0{Strptr}(p))  *)
        in stream_vt_nil() end
      end 
      : stream_vt_con(elt)
      (* , *)
      (* strptr_free($UN.castvwtp0{Strptr}(p)) *)
    )
in
  let 
    val x = auxmain(strptr2ptr(str))//, str)
    val () = strptr_free(str)
  in
    x
  end
  (* auxmain(string2ptr(str)) *)
end // end of [streamize_string_char]



(* ****** ****** *)

(* ****** ****** *)

fun
pipe_stream_vt2
  (cmd: string): stream_vt(charNZ) = svt where 
{
val rfp = popen_exn(cmd, $UN.cast{pmode(r)}("r"))

val svt =
(
let
fun loop(res: stream_vt(charNZ)): stream_vt(charNZ) =
  let
    val str = fgets0_gc(2, rfp)
    val () = assertloc (strptr2ptr(str) > 0)
    val isempty = strptr_is_empty(str)
  in
    ifcase
    | not(isempty) => let
       val y = streamize_strptr_char(str)
       (* val y2 = tostring_int(length(y)) *)
       (* val () = olor(cyan, y2) *)
       (* val () = strptr_free(str) *)
       val x = stream_vt_append(res, loop(y)) : stream_vt(charNZ)
      in  x
      end

    | _ => let
        val () = strptr_free(str)
       in res
       end
  end
in
    loop(stream_vt_make_nil())
end
): stream_vt(charNZ)

val status = pclose0_exn(rfp)
}
