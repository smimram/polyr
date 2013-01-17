let interactive = ref false

exception Timeout

let () =
  Printexc.record_backtrace true

let timeout sec f x =
  let set_timer sec =
    ignore
      (Unix.setitimer
         Unix.ITIMER_REAL
         { Unix.it_interval = 0.; Unix.it_value = sec })
  in
  let oldsig = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  set_timer sec;
  try
    let ans = f x in
    set_timer 0.;
    Sys.set_signal Sys.sigalrm oldsig;
    ans
  with
  | e ->
    set_timer 0.;
    Sys.set_signal Sys.sigalrm oldsig;
    raise e

let () =
  Printf.printf "Welcome to polyr!\n%!";
  let fname = ref None in
  Arg.parse
    [
      "-i", Arg.Set interactive, " Interactive mode.";
      "--interactive", Arg.Set interactive, " Interactive mode.";
    ]
    (fun f -> fname := Some f)
    "polyr [options] [file]";
  let ic =
    match !fname with
    | Some f -> open_in f
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
	let l = if l <> "" && l.[String.length l - 1] = '\r' then String.sub l 0 (String.length l - 1) else l in
        if !fname <> None then Printf.printf "%s# %s\n%!" (now ()) l;
        (
          try
            if Sys.os_type = "Win32" then Lang.cmd l
            else timeout !Lang.timeout Lang.cmd l
          with
          | Timeout -> Printf.printf "Timeout!\n%!"
          | Failure s -> Printf.printf "Error: %s\n%!" s
          | e -> Printf.printf "%s\n%s\n%!" (Printexc.to_string e) (Printexc.get_backtrace ())
        )
      with
      | End_of_file ->
        close_in !ic;
        if !interactive then
          (
            fname := None;
            ic := stdin
          )
        else raise Exit
    done
  with
  | Exit -> ()
