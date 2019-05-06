open Core

type ('a, 'b) t = ('a * 'b) list

type ('a, 'b) map = ('a, 'b) t

let empty = []

let of_list l = l

let to_list l = l

let fold_make f l =
  of_list (List.map f l)

let assoc = Listq.assoc
let app f x = assoc x f

let rec coassoc y = function
  | (x,y')::_ when y' == y -> x
  | _::l -> coassoc y l
  | [] -> raise Not_found

let mem f x =
  List.exists (fun (y,_) -> y == x) f

let add f x y =
  assert (not (mem f x));
  (x,y)::f

let rm f x =
  assert (mem f x);
  Listq.filter (fun (x',_) -> x' != x) f

let union f1 f2 =
  assert (Listq.for_all (fun x -> app f1 x == app f2 x) (Listq.inter (Listq.map fst f1) (Listq.map fst f2)));
  (* TODO: we should remove duplicates *)
  f1@f2

let map f l =
  Listq.map (fun (x,y) -> x, f y) l

let transform f l =
  Listq.map (fun (x,y) -> f x y) l

let seq f1 f2 =
  map (app f2) f1

let op l =
  transform (fun x y -> y,x) l

let lan f1 f2 =
  let f = ref empty in
  List.iter
    (fun (x,y) ->
     if mem !f y then
       assert (app !f y == app f1 x)
     else
       f := add !f y (app f1 x)
    ) f2;
  !f

let compatible f1 f2 =
  List.for_all
    (fun (x,y) ->
     List.for_all (fun (x',y') -> x != x' || y == y') f2
    ) f1

let restrict d f =
  Listq.filter (fun (x,_) -> Listq.mem x d) f

let domain f =
  List.map fst (to_list f)

let codomain f =
  List.map snd (to_list f)

let rec injective = function
  | (x,y)::f -> not (List.exists (fun (x',y') -> y == y') f) && injective f
  | [] -> true

let surjective d f =
  assert (Listq.included (codomain f) d);
  List.for_all (fun (x,y) -> Listq.mem y d) f

let bijective d f = injective f && surjective d f

(** Equivalence classes on elements with physical equality. *)
module Equiv = struct
  type 'a equiv = ('a,'a) t

  type 'a t = 'a equiv

  (** Empty relation. *)
  let empty = empty

  let repr r x =
    app r x

  let mem r x =
    mem r x

  (** Add an element to the domain of the relation. *)
  let add r x =
    assert (not (mem r x));
    add r x x

  (** Add a relation between two elements. *)
  let relate r x y =
    assert (mem r x);
    assert (mem r y);
    let x = repr r x in
    let y = repr r y in
    if x == y then r else
      map (fun y' -> if y' == x then y else y') r

  let domain = domain

  let map = id
end
