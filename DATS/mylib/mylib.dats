#ifndef TOKENS_NONE

  #define TOKENS_NONE
  
  #include "share/atspre_staload.hats"
  staload UN = "prelude/SATS/unsafe.sats"
  // #include "./bashstr.dats"

#endif

(* ****** ****** *)

#define MYLIB

local

staload "libats/libc/SATS/stdio.sats"

in

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

end


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
