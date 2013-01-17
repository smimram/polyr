open Stdlib

(** A stage of a polygraph, i.e. a list of homogeneous generators. *)
type stage = generator list
(** A generator. *)
and generator =
  {
    g_source : cell;
    g_target : cell;
    g_label : generator option;
  }
(** A cell. *)
and cell =
  {
    c_source : cell;
    c_target : cell;
    c_generators : cell list; (** the top dimensional generators of a cell *)
  }
(** A polygraph. *)
and polygraph =
  {
    stage : stage array; (** the stages of the polygraph ordered by dimension *)
  }

(** The empty polygraph. *)
let empty =
  {
    stage = [||];
  }

let create () = empty

(** The (-1)-dimensional cell. *)
let rec inicell =
  {
    c_source = inicell;
    c_target = inicell;
    c_generators = [];
  }

(** Add a generator to a polygraph. *)
let add_generator poly dim ?label src tgt =
  (* TODO: assert globular identities *)
  let cell =
    {
      g_source = src;
      g_target = tgt;
      g_label = label;
    }
  in
  let stage = Array.expand poly.stage [] dim in
  let stage = Array.replace stage dim (cell::stage.(dim)) in
  {
    stage;
  }

(** Names for cells of a polygraph. *)
module Namer = struct
  type t = (generator * string) list

  let create () = []

  let add n g x = (g,x)::n

  let name n g = List.assq g n
end
