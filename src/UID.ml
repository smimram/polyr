(** On-the-fly unique identifier generator. *)

type 'a t =
    {
      mutable counter : int;
      mutable map : ('a,int) Mapq.t
    }

(** Create an UID generator. *)
let create () =
  {
    counter = (-1);
    map = Mapq.empty;
  }

(** Retrieve the UID of a generator. *)
let get uid g =
  try
    Mapq.app uid.map g
  with
  | Not_found ->
     uid.counter <- uid.counter + 1;
     uid.map <- Mapq.add uid.map g uid.counter;
     uid.counter

(** UID generator for named elements. *)
module Named = struct
  module SMap = Map.Make (struct type t = string let compare = compare end)

  type 'a uid = 'a t

  (** UID generator. *)
  type 'a t = ('a uid) SMap.t ref

  (** UID of a given generator. *)
  let get (uid:'a t) name g =
    if not (SMap.mem name !uid) then uid := SMap.add name (create ()) !uid;
    let uid = SMap.find name !uid in
    get uid g

  (** Create a new UID generator. *)
  let create () : 'a t = ref SMap.empty
end
