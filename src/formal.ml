(** Operations on formal expressions in free ω-categories. *)

(** A formal expression. *)
(* TODO: add structural morphisms too! *)
type cell =
  | Seq of cell * int * cell (** sequential composition in given dimension *)
  | Id of cell (** identity on a cell *)
  | Gen of generator (** generator *)
  | Struct of structural (** structural morphism *)
  | Unit (** the only expression of dimension -1 *)
 (** A generator. *)
 and generator =
   {
     g_dim : int;
     g_name : string;
     g_source : cell;
     g_target : cell;
   }
 (** A structural morphism. *)
 and structural =
   | Exchange of (cell * int * cell) * int * (cell * int * cell)
   | Idcomp of bool * (cell * int * cell) (** from the identity of a composite to the composite of identities (or inverse if the boolean is true) *)

let g_dim g = g.g_dim

let g_name g = g.g_name

let g_source g = g.g_source

let g_target g = g.g_target

let rec c_dim = function
  | Seq (e1,_,e2) ->
     let d1 = c_dim e1 in
     let d2 = c_dim e2 in
     assert (d1 = d2);
     d1
  | Id e -> c_dim e + 1
  | Gen g -> g_dim g
  | Struct (Exchange((e1,_,e2),_,(e3,_,e4))) ->
     let d1 = c_dim e1 in
     let d2 = c_dim e2 in
     let d3 = c_dim e3 in
     let d4 = c_dim e4 in
     assert (d1 = d2 && d2 = d3 && d3 = d4);
     d1 + 1
  | Struct (Idcomp(_,(e1,_,e2))) ->
     let d1 = c_dim e1 in
     let d2 = c_dim e2 in
     assert (d1 = d2);
     d1 + 1
  | Unit -> -1

let rec c_source = function
  | Seq (e1,d,e2) ->
     if c_dim e1 = d+1 then c_source e1
     else Seq (c_source e1, d, c_source e2)
  | Id e -> e
  | Gen g -> g_source g
  | Struct (Exchange ((e1,i,e2),j,(e3,i',e4))) ->
     Seq(Seq(e1,i,e2),j,Seq(e3,i',e4))
  | Struct (Idcomp(b,(e1,i,e2))) ->
     if b then Seq(Id e1,i,Id e2)
     else Id(Seq(e1,i,e2))
  | Unit -> Unit

let rec c_target = function
  | Seq (e1,d,e2) ->
     if c_dim e1 = d+1 then c_target e2
     else Seq (c_target e1, d, c_target e2)
  | Id e -> e
  | Gen g -> g_target g
  | Struct (Exchange ((e1,i,e2),j,(e3,i',e4))) ->
     assert (i = i');
     Seq(Seq(e1,j,e3),i,Seq(e2,j,e4))
  | Struct (Idcomp(b,(e1,i,e2))) ->
     if b then Id(Seq(e1,i,e2))
     else Seq(Id e1,i,Id e2)
  | Unit -> Unit

module Generator = struct
  type t = generator

  let dim = g_dim

  let name = g_name

  let source = g_source

  let target = g_target

  let create ~name ~source ~target () =
    let dim = c_dim source + 1 in
    assert (c_dim target + 1 = dim);
    assert (c_source source = c_source target);
    assert (c_target source = c_target target);
    {
      g_dim = dim;
      g_name = name;
      g_source = source;
      g_target = target;
    }
end
module G = Generator

module Cell = struct
  type t = cell

  let dim = c_dim

  let source = c_source

  let target = c_target

  let unit = Unit
  let dummy = unit

  let generator g = Gen g

  let id c = Id c

  let seq d c1 c2 = Seq (c1,d,c2)
end
module C = Cell

(** Operations of signatures. *)
module Signature = struct
  (** A signature. *)
  type t =
      {
        dim : int;
        generators : generator list;
        prev : t;
      }

  let dim s = s.dim

  let generators s = s.generators

  let prev s = s.prev

  let rec dummy =
    {
      dim = -1;
      generators = [];
      prev = dummy;
    }

  let degenerate s =
    {
      dim = dim s + 1;
      generators = [];
      prev = s;
    }

  let add s g =
    assert (dim s = G.dim g);
    assert (not (List.exists (fun g' -> G.name g' = G.name g) (generators s)));
    { s with generators = g::(generators s) }

  let adds s gg =
    List.fold_left add s gg
end
module S = Signature

let () =
  if true then
    (
      Printexc.record_backtrace true;
      let s0 = S.degenerate S.dummy in
      let star = G.create ~name:"✶" ~source:C.dummy ~target:C.dummy () in
      let star_c = C.generator star in
      let s0 = S.add s0 star in
      let s1 = S.degenerate s0 in
      let one = G.create ~name:"a" ~source:star_c ~target:star_c () in
      let one_c = C.generator one in
      let s1 = S.add s1 one in
      let s2 = S.degenerate s1 in
      let mu = G.create ~name:"µ" ~source:(C.seq 0 one_c one_c) ~target:one_c () in
      let eta = G.create ~name:"eta" ~source:(C.id star_c) ~target:one_c () in
      let s2 = S.adds s2 [mu;eta] in
      ()
    )
