open Stdlib

module P = Polygraph

let timeout = ref 5.

let p = ref (P.empty)
let undo = ref []

let cmd ?(print=print_string) cmd =
  let printf fmt = Printf.ksprintf print fmt in
  let print_poly () = () in
  printf "Command was: %s\n%!" cmd;
  let rec handlers =
    [
      "help", ("Display this help.", 0, fun p a ->
        let handlers = List.sort (fun (n1,_) (n2,_) -> compare n1 n2) handlers in
        let cmd = List.map (fun (name,(help,_,_)) -> Printf.sprintf "- %s: %s\n" name help) handlers in
        let cmd = String.concat "" cmd in
        printf "Available commands:\n%s%!" cmd;
        p);
      "undo", ("Undo last operation.", 0, fun p a ->
        if !undo = [] then p
        else
          let p = List.hd !undo in
          undo := List.tl !undo;
          p);
      "save", ("Save current rewriting system in a file.", 1, fun p a ->
        let oc = open_out a.(0) in
        close_out oc;
	p);
      "quit", ("Quit the program.", 0, fun rs a -> exit 0);
      "exit", ("Quit the program.", 0, fun rs a -> exit 0);
      "timeout", ("Set timeout duration (in seconds).", 1, fun p a ->
	timeout := float_of_string a.(0);
	p);
    ]
  in
  let cmd = String.split_char cmd ' ' in
  let cmd = List.filter (fun s -> s <> "") cmd in
  match cmd with
  | cmd::_ when cmd.[0] = '#' -> ()
  | cmd::args ->
    let _, a, f =
      try
        List.assoc cmd handlers
      with
      | Not_found -> failwith (Printf.sprintf "Unknown command %s." cmd)
    in
    let args = Array.of_list args in
    if Array.length args <> a then failwith (Printf.sprintf "Command %s requires %d arguments." cmd a);
    let pold = !p in
    p := f !p args;
    if !p <> pold then
      (
        if cmd <> "undo" then undo := pold :: !undo;
        print_poly ()
      )
  | [] -> ()
