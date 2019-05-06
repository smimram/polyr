(** Globular sets and the categories they generate. *)

open Core

module Equivq = Mapq.Equiv

module String = struct
  include String

  let tabs n = String.make (2*n) ' '
end

(** Operations on globular sets. *)
module Set = struct
  (** Operations on generators. *)
  module Generator = struct
    (** An element of a globular set. *)
    type t =
        {
          dim : int;
          name : string; (** name, only for printing purposes *)
          label : t option; (** label of the generator *)
          source : t;
          target : t;
        }

    (** Dummy generator. *)
    let rec dummy =
      {
        dim = -1;
        name = "dummy";
        label = None;
        source = dummy;
        target = dummy;
      }

    let dim g = g.dim

    let name g = g.name

    let source g = g.source

    let target g = g.target

    let labeled g = g.label <> None

    let label g = Option.get g.label

    let same_labeling g1 g2 =
      labeled g1 = labeled g2 && (try label g1 == label g2 with Not_found -> true)

    let create ?name ?label ~source ~target () =
      assert (dim source = dim target);
      (match label with Some label -> assert (dim label = dim source + 1) | None -> ());
      let name =
        match name with
        | Some name -> name
        | None -> (Option.get label).name
      in
      let dim = dim source + 1 in
      { dim; name; label; source; target }

    module UID = struct
      type generator = t

      include UID.Named

      let create () : generator t = create ()

      let get uid g = get uid (name g) g

      let string uid g =
        name g ^ string_of_int (get uid g)

      let string uid g =
        let label = try "(" ^ string uid (label g) ^ ")" with Not_found -> "" in
        string uid g ^ label
    end

    let rec to_string uid g =
      if dim g = 0 then UID.string uid g else
        Printf.sprintf "%s : %s → %s" (UID.string uid g) (UID.string uid (source g)) (UID.string uid (target g))
  end
  module G = Generator

  let uidebug = G.UID.create ()

  (** A globular set. *)
  type t =
      {
        dim : int;
        generators : G.t list;
        prev : t;
      }

  type set = t

  let rec dummy =
    {
      dim = -1;
      generators = [];
      prev = dummy
    }
  let s_dummy = dummy

  let dim s = s.dim
  let s_dim = dim

  let generators s = s.generators

  let prev s = s.prev
  let s_prev = prev

  let create ~generators ~prev () =
    let dim = dim prev + 1 in
    assert (List.for_all (fun g -> G.dim g = dim) generators);
    { dim; generators; prev }

  (** Equality between globular sets. *)
  let rec eq s1 s2 =
    assert (dim s1 = dim s2);
    (s1 == s2) || (Listq.equiv (generators s1) (generators s2) && eq (prev s1) (prev s2))

  (** Add a new generator. *)
  let add s g =
    assert (G.dim g = dim s);
    assert (not (Listq.mem g (generators s)));
    (* We insert at the end only because it is nicer when printing. *)
    { s with generators = (generators s)@[g] }

  (** Add multiple generators. *)
  let adds s gg =
    List.fold_left add s gg

  let rec to_string ?(tab=0) uid s =
    let tabs = String.tabs tab in
    if dim s < 0 then "" else
      Printf.sprintf
        "%s%s- %d-cells:\n%s" (to_string ~tab uid (prev s)) tabs (dim s)
        (String.concat_map "" (fun g -> tabs ^ "  - " ^ G.to_string uid g ^ "\n") (generators s))

  let degenerate s =
    create ~generators:[] ~prev:s ()

  (** A morphism between globular sets. *)
  module Morphism = struct
    (* Cope with masking. *)
    let s_dummy = dummy
    let s_dim = dim
    let s_prev = prev
    let s_create = create

    type t =
        {
          dim : int;
          map : (Generator.t,Generator.t) Mapq.t;
          prev : t;
          source : set;
          target : set;
        }

    let rec dummy =
      {
        dim = -1;
        map = Mapq.empty;
        prev = dummy;
        source = s_dummy;
        target = s_dummy;
      }

    let dim f = f.dim

    let map f = f.map

    let prev f = f.prev

    let source f = f.source

    let target f = f.target

    let create ~map ~prev ~source ~target () =
      let dim = dim prev + 1 in
      assert (s_dim source = dim);
      assert (s_dim target = dim);
      assert (List.for_all (fun (g1,g2) -> G.same_labeling g1 g2) (Mapq.to_list map));
      { dim; map; prev; source; target }

    let rec to_string uid f =
      if dim f < 0 then "" else
        let s = to_string uid (prev f) in
        s ^ String.concat_map " " (fun (g1,g2) -> Printf.sprintf "%s ↦ %s" (G.to_string uid g1) (G.to_string uid g2)) (Mapq.to_list (map f)) ^ "\n"

    let degenerate f =
      create ~map:Mapq.empty ~prev:f ~source:(degenerate (source f)) ~target:(degenerate (target f)) ()

    (** Sequental composition of two morphisms. *)
    let rec seq f g =
      assert (dim f = dim g);
      if dim f < 0 then dummy else
        let map = Mapq.seq (map f) (map g) in
        let prev = seq (prev f) (prev g) in
        create ~map ~prev ~source:(source f) ~target:(target g) ()

    (** Apply a morphism to a top-dimensional generator. *)
    let app f g =
      assert (dim f = G.dim g);
      Mapq.app (map f) g

    (** Image of a set under a morphism. *)
    let rec set f s =
      assert (dim f = s_dim s);
      if s_dim s < 0 then s_dummy else
        let prev = set (prev f) (s_prev s) in
        let generators = Listq.unique (Listq.map (app f) (generators s)) in
        s_create ~generators ~prev ()

    (** Inclusion (strict) of a globular set into another one. *)
    let rec inclusion s1 s2 =
      assert (s_dim s1 = s_dim s2);
      if s_dim s1 < 0 then dummy else
        let f' = inclusion (s_prev s1) (s_prev s2) in
        assert (Listq.included (generators s1) (generators s2));
        let map = Mapq.of_list (List.map (fun g -> g,g) (generators s1)) in
        create ~map ~prev:f' ~source:s1 ~target:s2 ()

    (** Identity morphism. *)
    let id s =
      inclusion s s

    (** Universal extension of two maps on a corpoduct. *)
    let rec coprod f1 (i1,i2) f2 =
      assert (dim f1 = dim i1);
      assert (dim i1 = dim i2);
      assert (dim i2 = dim f2);
      assert (eq (target i1) (target i2));
      assert (eq (target f1) (target f2));
      if dim f1 < 0 then dummy else
        let prev = coprod (prev f1) (prev i1, prev i2) (prev f2) in
        let f1' = Mapq.lan (map f1) (map i1) in
        let f2' = Mapq.lan (map f2) (map i2) in
        let map = Mapq.union f1' f2' in
        create ~map ~prev ~source:(target i1) ~target:(target f1) ()
  end
  module M = Morphism

  (** Operations on partial morphisms. *)
  module PMorphism = struct
    type t =
        {
          dim : int;
          mutable map : (Generator.t,Generator.t) Mapq.t;
          prev : t;
          source : set;
          target : set;
        }

    let rec dummy =
      {
        dim = -1;
        map = Mapq.empty;
        prev = dummy;
        source = s_dummy;
        target = s_dummy
      }

    let dim f = f.dim

    let map f = f.map

    let prev f = f.prev

    let source f = f.source

    let target f = f.target

    (** Create a morphism undefined everywhere. *)
    let rec create source target =
      assert (s_dim source = s_dim target);
      let dim = s_dim source in
      if dim < 0 then dummy else
        let map = Mapq.empty in
        let prev = create (s_prev source) (s_prev target) in
        { dim; map; prev; source; target; }

    (** Create a morphism from a (total) partial morphism. *)
    let rec morphism f =
      if dim f < 0 then M.dummy else
        let prev = morphism (prev f) in
        let source = source f in
        let target = target f in
        let map = map f in
        assert (List.for_all (Mapq.mem map) (generators source));
        M.create ~map ~prev ~source ~target ()

    (** Apply a partial morphism to a generator. *)
    let app f g =
      assert (0 <= G.dim g && G.dim g <= dim f);
      let f = iterate (dim f - G.dim g) prev f in
      Option.find (Mapq.app (map f)) g

    (** Add a binding from a generator to another. *)
    let add f g1 g2 =
      assert (G.dim g1 = G.dim g2);
      assert (G.same_labeling g1 g2);
      match app f g1 with
      | Some g2' -> assert (g2 == g2')
      | None ->
         let f = iterate (dim f - G.dim g1) prev f in
         f.map <- Mapq.add (map f) g1 g2

    (** Remove a binding. *)
    let rm f g =
      assert (0 <= G.dim g && G.dim g <= dim f);
      let f = iterate (dim f - G.dim g) prev f in
      f.map <- Mapq.rm (map f) g

    (** n-dimensional generators of the target. *)
    let cogenerators f n =
      assert (0 <= n && n <= dim f);
      let p = iterate (dim f - n) s_prev (target f) in
      generators p

    (** n-dimensional generators of the source. *)
    let generators f n =
      assert (0 <= n && n <= dim f);
      let p = iterate (dim f - n) s_prev (source f) in
      generators p
  end

  (** Equivalence relations on globular sets. *)
  module Quotient = struct
    let s_dim = dim
    let s_prev = prev
    let s_create = create
    let s_dummy = dummy

    type t =
        {
          set : set;
          equiv : G.t Equivq.t; (** equivalence relation between n-cells *)
          prev : t;
        }

    let set r = r.set

    let dim r = dim (set r)

    let equiv r = r.equiv

    let prev r = r.prev

    let rec dummy =
      {
        set = s_dummy;
        equiv = Equivq.empty;
        prev = dummy;
      }

    let create ~set ~equiv ~prev () =
      { set; equiv; prev }

    (** Canonical representative of a generator. *)
    let repr r g =
      assert (dim r = G.dim g);
      if dim r < 0 then G.dummy else
        Equivq.repr (equiv r) g

    let degenerate ?set r =
      assert (match set with Some set -> s_dim set = dim r + 1 && eq (s_prev set) r.set | None -> true);
      let set = Option.default (degenerate r.set) set in
      let equiv = List.fold_left Equivq.add Equivq.empty (generators set) in
      create ~set ~equiv ~prev:r ()

    (** Add a relation between two generators. *)
    let add r g1 g2 =
      (* Printf.printf "ADD RELATION: %s with %s\n%!" (G.to_string uidebug g1) (G.to_string uidebug g2); *)
      assert (dim r = G.dim g1);
      assert (dim r = G.dim g2);
      assert (G.same_labeling g1 g2);
      assert (Listq.mem g1 (generators (set r)));
      assert (Listq.mem g2 (generators (set r)));
      let equiv = equiv r in
      let prev = prev r in
      let g =
        let source = repr prev (G.source g1) in
        let target = repr prev (G.target g1) in
        G.create ~name:(G.name g1) ?label:(Option.find G.label g1) ~source ~target ()
      in
      let equiv = Equivq.add equiv g in
      let equiv = Equivq.relate equiv g1 g in
      let equiv = Equivq.relate equiv g2 g in
      create ~set:(set r) ~equiv ~prev ()

    (** Quotient of a globular set by an equivalence relation. *)
    let rec set r =
      let dim = dim r in
      if dim < 0 then M.id s_dummy else
        let s = r.set in
        let prev = set (prev r) in
        let map = Listq.map (fun g -> g, repr r g) (generators s) in
        let map = Mapq.of_list map in
        let generators = Listq.unique (Listq.map (Mapq.app map) (generators s)) in
        let s' = s_create ~generators ~prev:(M.target prev) () in
        M.create ~map ~prev ~source:s ~target:s' ()
  end
  module Q = Quotient

  (** Inject a globular set into a copy of this globular set. *)
  let rec copy s =
    let dim = dim s in
    if dim < 0 then M.dummy else
      let f' = copy (prev s) in
      let g_copy g =
        let source, target =
          if G.dim g = 0 then G.dummy, G.dummy
          else M.app f' (G.source g), M.app f' (G.target g)
        in
        G.create ~name:(G.name g) ?label:(Option.find G.label g) ~source ~target ()
      in
      let map = Mapq.of_list (List.map (fun g -> g, g_copy g) (generators s)) in
      let generators = List.map (Mapq.app map) (generators s) in
      let target = create ~generators ~prev:(M.target f') () in
      let f = M.create ~map ~prev:f' ~source:s ~target () in
      f

  (** Non-disjoint union. *)
  let rec union s1 s2 =
    assert (dim s1 = dim s2);
    let dim = dim s1 in
    if dim < 0 then dummy else
      let prev = union (prev s1) (prev s2) in
      let generators = Listq.union (generators s1) (generators s2) in
      create ~generators ~prev ()

  (** Coproduct. *)
  let coprod s1 s2 =
    (* Printf.printf "COPROD\n%sWITH\n%s%!" (to_string uidebug s1) (to_string uidebug s2); *)
    let i1 = copy s1 in
    let i2 = copy s2 in
    let s1' = M.target i1 in
    let s2' = M.target i2 in
    let s = union s1' s2' in
    (* Printf.printf "IS\n%s%!" (to_string uidebug s); *)
    let i1' = M.inclusion s1' s in
    let i2' = M.inclusion s2' s in
    M.seq i1 i1', M.seq i2 i2'

  (** Coequalizer of two maps. *)
  let coeq f1 f2 =
    (* Printf.printf *)
      (* "COEQ\nFROM\n%sTO\n%s\nMAP1\n%sMAP2\n%s" *)
      (* (to_string uidebug (M.source f1)) *)
      (* (to_string uidebug (M.target f1)) *)
      (* (M.to_string uidebug f1) *)
      (* (M.to_string uidebug f2); *)
    assert (M.dim f1 = M.dim f2);
    assert (eq (M.source f1) (M.source f2));
    assert (eq (M.target f1) (M.target f2));
    let rec equiv f1 f2 =
      if M.dim f1 < 0 then Q.dummy else
        let r = equiv (M.prev f1) (M.prev f2) in
        let r = Q.degenerate ~set:(M.target f1) r in
        let r = List.fold_left (fun r g -> Q.add r (M.app f1 g) (M.app f2 g)) r (generators (M.source f1)) in
        (* This is necessary in order to send non-quotiented generators to
        copies with normal forms as source and target. *)
        List.fold_left (fun r g -> Q.add r g g) r (generators (M.target f1))
    in
    let e = equiv f1 f2 in
    let s = M.target f1 in
    Q.set e

  (** Pushout of two maps. *)
  let pushout f1 f2 =
    (* Printf.printf "PUSHOUT:\n%sAND\n%s%!" (to_string uidebug (M.target f1)) (to_string uidebug (M.target f2)); *)
    assert (M.dim f1 = M.dim f2);
    assert (eq (M.source f1) (M.source f2));
    let i1, i2 = coprod (M.target f1) (M.target f2) in
    let g = coeq (M.seq f1 i1) (M.seq f2 i2) in
    M.seq i1 g, M.seq i2 g
end
module S = Set
module G = Set.Generator
module M = Set.Morphism
module PM = Set.PMorphism

let uidebug = S.uidebug

(** Operations on cells of the free omega-category generated by a globular
set. *)
module Cell = struct
  (** A cell in the omega-category generated by a globular set. *)
  type t =
      {
        set : Set.t; (** underlying globular set *)
        source : t; (** source (n-1)-cell *)
        target : t; (** target (n-1)-cell *)
      }

  (** Globular set representing a cell. *)
  let set c = c.set

  (** Dimension of a cell. *)
  let dim c = S.dim (set c)

  (** Source of a cell. *)
  let source c = c.source

  (** Target of a cell. *)
  let target c = c.target

  (** Inclusion of the source into the globular set. *)
  let source_morphism ?(k=1) c =
    let source = iterate k source c in
    let s = iterate k S.degenerate (set source) in
    M.inclusion s (set c)

  (** Inclusion of the target into the globular set. *)
  let target_morphism ?(k=1) c =
    let target = iterate k target c in
    let s = iterate k S.degenerate (set target) in
    M.inclusion s (set c)

  let rec dummy =
    {
      set = S.dummy;
      source = dummy;
      target = dummy;
    }

  let create ~set ~source ~target () =
    assert (dim source + 1 = S.dim set);
    assert (dim target + 1 = S.dim set);
    { set; source; target }

  (** Identity cell on another cell. *)
  let id c =
    create ~set:(S.degenerate (set c)) ~source:c ~target:c ()

  (** Add a generator to a cell (use with care). *)
  let add c g =
    let set = S.add (set c) g in
    create ~set ~source:(source c) ~target:(target c) ()

  let generator_of_zero_cell c =
    assert (dim c = 0);
    List.element (S.generators (set c))

  let list_of_one_cell c =
    (* Printf.printf *)
      (* "LIST OF 1-CELL: %s -> %s\n%s\n%!" *)
      (* (\* (S.to_string uidebug (set (source c))) *\)"" *)
      (* (\* (S.to_string uidebug (set (target c))) *\)"" *)
      (* (S.to_string uidebug (set c)); *)
    assert (dim c = 1);
    let source = generator_of_zero_cell (source c) in
    let target = generator_of_zero_cell (target c) in
    let target = ref target in
    let l = ref [] in
    while !target != source do
      let g = List.find (fun g -> G.target g == !target) (S.generators (set c)) in
      l := (g, !target) :: !l;
      target := G.source g
    done;
    source, !l

  let rec to_string ?(tab=0) ?(human=true) uid c =
    let to_string ?(tab=tab) ?(human=human) uid c = to_string ~tab ~human uid c in
    let tabs = String.tabs tab in
    let dim = dim c in
    if dim < 0 then ""
    else if dim = 0 then
      let g = generator_of_zero_cell c in
      if human then
        Printf.sprintf "%s" (G.UID.string uid g)
      else
        Printf.sprintf "%s0-cell: %s\n" tabs (G.UID.string uid g)
    else if human && dim = 1 then
      let s, l = list_of_one_cell c in
      Printf.sprintf "%s" (G.UID.string uid s)
      ^ String.concat_map "" (fun (g,t) -> Printf.sprintf " -%s→ %s" (G.UID.string uid g) (G.UID.string uid t)) l
    else
      Printf.sprintf
        "%s%d-cell:\n%swith source\n%swith target\n%s" tabs dim
        (S.to_string ~tab:(tab+1) uid (set c))
        (String.ensure_nl (to_string ~tab:(tab+1) uid (source c)))
        (String.ensure_nl (to_string ~tab:(tab+1) uid (target c)))

  (** Create a cell contaning only one generator of a given globular set. *)
  let of_generator s g =
    assert (S.dim s = G.dim g);
    assert (Listq.mem g (S.generators s));
    assert (G.dim g >= 0);
    if G.dim g = 0 then
      let g' = G.create ~label:g ~source:G.dummy ~target:G.dummy () in
      let set = S.add (S.degenerate S.dummy) g' in
      create ~set ~source:dummy ~target:dummy ()
    else
      let rec sphere sgn s t =
        if G.dim s = 0 then
          let set = S.degenerate S.dummy in
          let s = G.create ~label:s ~source:G.dummy ~target:G.dummy () in
          let t = G.create ~label:t ~source:G.dummy ~target:G.dummy () in
          let set = S.add set s in
          let set = S.add set t in
          let set = S.degenerate set in
          let source = create ~set:(S.add (S.degenerate S.dummy) s) ~source:dummy ~target:dummy () in
          let target = create ~set:(S.add (S.degenerate S.dummy) t) ~source:dummy ~target:dummy () in
          create ~set ~source ~target ()
        else
          let c = sphere (S.prev sgn) (G.source s) (G.target t) in
          let gsrc' = List.element (S.generators (set (source c))) in
          let gtgt' = List.element (S.generators (set (target c))) in
          let gsrc = G.create ~label:s ~source:gsrc' ~target:gtgt' () in
          let gtgt = G.create ~label:t ~source:gsrc' ~target:gtgt' () in
          let source = add c gsrc in
          let target = add c gtgt in
          let set = set c in
          let set = S.add set gsrc in
          let set = S.add set gtgt in
          let set = S.degenerate set in
          create ~set ~source ~target ()
      in
      let c = sphere (S.prev s) (G.source g) (G.target g) in
      let source = List.element (S.generators (set (source c))) in
      let target = List.element (S.generators (set (target c))) in
      let g = G.create ~label:g ~name:(G.name g) ~source ~target () in
      add c g

  (** Canonical isomorphism between two cells. *)
  let identify c1 c2 =
    (* Printf.printf "IDENTIFY %s WITH %s\n%!" (to_string uidebug c1) (to_string uidebug c2); *)
    assert (dim c1 = dim c2);
    assert (dim c1 >= 0);
    let f = PM.create (set c1) (set c2) in
    let q = Queue.create () in
    let push g1 g2 = Queue.push (g1,g2) q in
    let rec aux c1 c2 =
      if dim c1 = 0 then
        let g1 = generator_of_zero_cell c1 in
        let g2 = generator_of_zero_cell c2 in
        PM.add f g1 g2
      else
        (
          aux (source c1) (source c2);
          aux (target c1) (target c2);
          let l = Mapq.to_list (PM.map (iterate (PM.dim f - dim c1 + 1) PM.prev f)) in
          List.iter (fun (g1,g2) -> PM.rm f g1; push g1 g2) l;
          while not (Queue.is_empty q) do
            let g1, g2 = Queue.pop q in
            (* Printf.printf "match %s with %s\n%!" (G.to_string uidebug g1) (G.to_string uidebug g2); *)
            match PM.app f g1 with
            | Some g2' -> assert (g2 == g2')
            | None ->
               PM.add f g1 g2;
               if G.dim g1 <> 0 then
                 (
                   push (G.source g1) (G.source g2);
                   push (G.target g1) (G.target g2);
                 );
               if G.dim g1 < dim c1 then
                 (
                   let gg1 = PM.generators f (G.dim g1 + 1) in
                   let gg2 = PM.cogenerators f (G.dim g2 + 1) in
                   let s1 = Option.find (Listq.find (fun g -> G.source g == g1)) gg1 in
                   let s2 = Option.find (Listq.find (fun g -> G.source g == g2)) gg2 in
                   let t1 = Option.find (Listq.find (fun g -> G.target g == g1)) gg1 in
                   let t2 = Option.find (Listq.find (fun g -> G.target g == g2)) gg2 in
                   (
                     match s1,s2 with
                     | Some s1, Some s2 -> push s1 s2
                     | None, None -> ()
                     | _ -> assert false
                   );
                   (
                     match t1,t2 with
                     | Some t1, Some t2 -> push t1 t2
                     | None, None -> ()
                     | _ -> assert false
                   )
                 )
          done
        )
    in
    aux c1 c2;
    PM.morphism f

  (** Map a morphism onto a cell. *)
  let rec map f c =
    assert (M.dim f = dim c);
    if dim c < 0 then dummy else
      let set = M.set f (set c) in
      let source = map (M.prev f) (source c) in
      let target = map (M.prev f) (target c) in
      create ~set ~source ~target ()

  (** Sequential composition of two cells. *)
  let seq d c1 c2 =
    assert (dim c1 = dim c2);
    assert (0 <= d && d < dim c1);
    let k = dim c1 - d in
    let t1 = iterate k target c1 in
    let s2 = iterate k source c2 in
    let f = identify t1 s2 in
    let f1, f2 = S.pushout (target_morphism ~k c1) (M.seq (iterate k M.degenerate f) (source_morphism ~k c2)) in
    let s = M.target f1 in
    (* Recusive composition of the sources and targets has to reuse the already
    computed maps. *)
    let rec seq c1 f1 c2 f2 =
      let k = dim c1 - d in
      let source, target =
        (
          if k = 1 then map f1 (source c1)
          else seq (source c1) (M.prev f1) (source c2) (M.prev f2)
        ), (
          if k = 1 then map f2 (target c2)
          else seq (source c1) (M.prev f1) (source c2) (M.prev f2)
        )
      in
      let set = S.union (M.set f1 (set c1)) (M.set f2 (set c2)) in
      create ~set ~source ~target ()
    in
    let source, target =
      (
        if k = 1 then map (M.prev f1) (source c1)
        else seq (source c1) (M.prev f1) (source c2) (M.prev f2)
      ), (
        if k = 1 then map (M.prev f2) (target c2)
        else seq (source c1) (M.prev f1) (source c2) (M.prev f2)
      )
    in
    create ~set:s ~source ~target ()
end
module C = Cell

let () =
  Printexc.record_backtrace true;
  let s0 = S.degenerate S.dummy in
  let a = G.create ~name:"A" ~source:G.dummy ~target:G.dummy () in
  let b = G.create ~name:"B" ~source:G.dummy ~target:G.dummy () in
  let s0 = S.adds s0 [a;b] in
  let s1 = S.degenerate s0 in
  let f = G.create ~name:"f" ~source:a ~target:b () in
  let g = G.create ~name:"g" ~source:a ~target:b () in
  let h = G.create ~name:"h" ~source:b ~target:b () in
  let s1 = S.adds s1 [f;g;h] in
  let s2 = S.degenerate s1 in
  let alpha = G.create ~name:"α" ~source:f ~target:g () in
  let beta = G.create ~name:"β" ~source:g ~target:g () in
  let s2 = S.adds s2 [alpha;beta] in
  Printf.printf "s:\n%s\n%!" (S.to_string uidebug s2);
  let cb = C.of_generator s0 b in
  let calpha = C.of_generator s2 alpha in
  let cbeta = C.of_generator s2 beta in
  Printf.printf "calpha:\n%s\n\n%!" (C.to_string uidebug calpha);
  Printf.printf "cbeta:\n%s\n\n%!" (C.to_string uidebug cbeta);
  (* let cf = C.of_generator s1 f in *)
  (* let cg = C.of_generator s1 g in *)
  (* let ch = C.of_generator s1 h in *)
  (* Printf.printf "cg: %s\n\n%!" (C.to_string uidebug cg); *)
  (* Printf.printf "ch: %s\n\n%!" (C.to_string uidebug ch); *)
  (* let cgh = C.seq 0 cg ch in *)
  (* Printf.printf "cgh:\n%s\n%!" (C.to_string uidebug cgh); *)
  (* let cgh = C.seq 0 cgh (C.id cb) in *)
  (* Printf.printf "cgh:\n%s\n%!" (C.to_string uidebug cgh); *)
  let calphabeta = C.seq 1 (C.seq 1 calpha cbeta) cbeta in
  Printf.printf "calphabeta:\n%s\n%!" (C.to_string uidebug calphabeta);
