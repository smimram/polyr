(** Standard library. *)

let id x = x

(** Least fixpoint with physical equality. *)
let rec lfpq f x =
  let x' = f x in
  if x' == x then x else lfpq f x'

(** Iterate of a function. *)
let rec iterate n f x =
  if n = 0 then x
  else iterate (n-1) f (f x)

exception Timeout

let timeout sec f x =
  if Sys.os_type = "Win32" then
    (
      let rec alarm =
        let target = Unix.gettimeofday () +. sec in
        Gc.create_alarm (fun () -> if Unix.gettimeofday () >= target then raise Timeout)
      in
      try
        let ans = f x in
        Gc.delete_alarm alarm;
        ans
      with
      | e -> Gc.delete_alarm alarm; raise e
    )
  else
    (
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
    )
  (*
  let ans = ref None in
  let f x = ans := Some (f x) in
  let tf = Thread.create f x in
  let timer () = Thread.delay sec; Thread.kill tf in
  let _ = Thread.create timer () in
  Thread.join tf;
  match !ans with
  | Some ans -> ans
  | None -> raise Timeout
  *)

module Option = struct
  let get = function
    | Some x -> x
    | None -> raise Not_found

  let default x = function
    | Some x -> x
    | None -> x

  let bind f = function
    | Some x -> f x
    | None -> None

  let may f = function
    | Some x -> f x
    | None -> ()

  let funct f = bind (fun x -> Some (f x))

  let find f x =
    try Some (f x) with Not_found -> None
end

module Pair = struct
  let swap (x,y) = (y,x)
end

module String = struct
  include String

  let concat_map s f l =
    String.concat s (List.map f l)

  (** Ensure that a string ends with a newline. *)
  let ensure_nl s =
    if s = "" then "\n" else
      if s.[String.length s - 1] = '\n' then s else s ^ "\n"
end

module List = struct
  include List

  (** Bind for the free monoid monad. *)
  let bind f u =
    List.flatten (List.map f u)

  (** Unit for the free monoid monad. *)
  let unit x = [x]

  (** Return the element of a list with only one element. *)
  let element = function
    | [x] -> x
    | _ -> raise Not_found

  let map_context f l =
    let rec aux h = function
      | x::t -> (f h x t)::(aux (x::h) t)
      | [] -> []
    in
    aux [] l

  let rec may_map f = function
    | x::l ->
       (
         match f x with
         | Some x -> x::(may_map f l)
         | None -> may_map f l
       )
    | [] -> []
end

(** Same as [List] but with physical equality everywhere. *)
module Listq = struct
  let map = List.map
  let find = List.find
  let filter = List.filter
  let exists = List.exists
  let for_all = List.for_all

  let mem x l =
    exists (fun y -> y == x) l

  let remove x l =
    filter (fun y -> y != x) l

  let included l1 l2 =
    for_all (fun x -> mem x l2) l1

  let equiv l1 l2 =
    included l1 l2 && included l2 l1

  (** Remove duplicates. *)
  let rec unique = function
    | x::l -> if mem x l then unique l else x::(unique l)
    | [] -> []

  let union l1 l2 = unique (l1@l2)

  let rec is_unique = function
    | x::l -> if mem x l then false else is_unique l
    | [] -> true

  let inter l1 l2 =
    filter (fun x -> mem x l1) l2

  let assoc = List.assq
end

(** Enumerations. *)
module Enum = struct
  type 'a t = unit -> 'a

  exception End

  let empty =
    fun () -> raise End

  let get e = e ()

  (** Create an enum from a function returning the next element. The function
      should raise [End] if there are no more elements. *)
  let make f = f

  let append e1 e2 =
    fun () ->
    try
      get e1
    with
    | End -> get e2

  let map f e =
    fun () -> f (get e)

  let rec may_map f e =
    fun () ->
    match f (get e) with
    | Some x -> x
    | None -> get (may_map f e)

  let filter p e =
    let rec aux p e =
      let x = get e in
      if p x then
        x
      else
        aux p e
    in
    fun () -> aux p e

  let iter f e =
    try
      while true do
        f (get e)
      done
    with
    | End -> ()

  let fold f init e =
    let ans = ref init in
    try
      while true do
        ans := f !ans (get e)
      done;
      assert false
    with
    | End -> !ans

  let of_list l =
    let l = ref l in
    fun () ->
    if !l = [] then raise End else
      let x = List.hd !l in
      l := List.tl !l;
      x

  let to_list e =
    List.rev (fold (fun l x -> x::l) [] e)

  let reduce f e =
    try
      let init = get e in
      fold f init e
    with
    | End -> raise Not_found

  (** Indices from [0] to [n-1]. *)
  let indices n =
    let k = ref (-1) in
    fun () ->
    incr k;
    if !k < n then
      !k
    else
      raise End
end
