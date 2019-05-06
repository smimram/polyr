(** Interactive toplevel. *)

open Core

module G = Polygraph.G
module P = Polygraph.P
module C = Polygraph.C

(** Operations on environments. *)
module Env = struct
  type elt =
    | Polygraph of P.t
    | Cell of C.t

  type t = (string * elt) list

  let get e x = List.assoc x e

  let set e x v = (x,v) :: e
end

let cmd l =
  ()

(* Start topelevel *)

let interactive = ref true
let verbose = ref true
let () = Printexc.record_backtrace true
let () =
  let fname = ref None in
  Arg.parse
    [
     "-b", Arg.Clear interactive, " Batch mode.";
     "--batch", Arg.Clear interactive, " Batch mode.";
   ]
    (fun f -> fname := Some f)
    "rewr [options] [file]";
  let ic =
    match !fname with
    | Some f -> verbose := false ; open_in f
    | None -> stdin
  in
  let ic = ref ic in
  let now () =
    (* let tm = Unix.localtime (Unix.time ()) in *)
    (* Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec *)
    ""
  in
  try
    while true do
      try
        if !fname = None then Printf.printf "%s# %!" (now ());
        let l = input_line !ic in	
	let l = 
	  if l <> "" && l.[String.length l - 1] = '\r' 
	  then String.sub l 0 (String.length l - 1) 
	  else l
	in
        if !fname <> None then Printf.printf "%s%!" (now ())(*Printf.printf "%s# %s\n%!" (now ()) l*);
        (
         try
           (* if Sys.os_type = "Win32" then Lang.cmd l else *)
           timeout 10. cmd l
         with
         | Timeout -> Printf.printf "\nTimeout!\n%!"
         | Failure s -> Printf.printf "Error: %s\n%!" s
         | e -> Printf.printf "%s\n%s\n%!" (Printexc.to_string e) (Printexc.get_backtrace ())
        )
      with
      | End_of_file ->
          close_in !ic;
          if !interactive then
            (
             fname := None;
	     verbose := true;
	     (* Lang.cmd !verbose "rs"; *)
	     (* Lang.cmd !verbose "order"; *)
             ic := stdin;
            )
          else raise Exit
    done
  with
  | Exit -> ()
