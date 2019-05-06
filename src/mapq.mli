(** A map between elements with physical equality. *)

type ('a,'b) t

type ('a,'b) map = ('a,'b) t

val empty : ('a,'b) t

val of_list : ('a*'b) list -> ('a,'b) t

val to_list : ('a,'b) t -> ('a*'b) list

val fold_make : ('a -> ('b * 'c)) -> 'a list -> ('b,'c) t

val domain : ('a,'b) t -> 'a list

val codomain : ('a,'b) t -> 'b list

val restrict : 'a list -> ('a,'b) t -> ('a,'b) t

(** Is the morphism defined on a value? *)
val mem : ('a,'b) t -> 'a -> bool

(** Application to an argument. *)
val app : ('a,'b) t -> 'a -> 'b

(** Same as [app] but with [List]-style order of arguments. *)
val assoc : 'a -> ('a,'b) t -> 'b

val coassoc : 'b -> ('a,'b) t -> 'a

val add : ('a,'b) t -> 'a -> 'b -> ('a,'b) t

val rm : ('a,'b) t -> 'a -> ('a,'b) t

(** Union of two maps which coincide on common elements. *)
val union : ('a,'b) t -> ('a,'b) t -> ('a,'b) t

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t

(** Map on both keys and values. *)
val transform : ('a -> 'b -> 'a*'b) -> ('a,'b) t -> ('a,'b) t

val seq : ('a,'b) t -> ('b,'c) t -> ('a,'c) t

val op : ('a,'b) t -> ('b,'a) t

(** Extension of a map along another one. *)
val lan : ('a, 'c) t -> ('a, 'b) t -> ('b, 'c) t

(** Are two maps compatible (i.e. if both are defined then they have the same
image) ? *)
val compatible : ('a,'b) t -> ('a,'b) t -> bool

(** Is map injective? *)
val injective : ('a,'b) t -> bool

(** Is map surjective on given codomain? *)
val surjective : 'b list -> ('a,'b) t -> bool

(** Is map bijective on given codomain? *)
val bijective : 'b list -> ('a,'b) t -> bool

module Equiv : sig
  type 'a t

  val empty : 'a t

  (** Quotient map. *)
  val map : 'a t -> ('a,'a) map

  val domain : 'a t -> 'a list

  (** Canonical representative. *)
  val repr : 'a t -> 'a -> 'a

  (** Add an element to the domain of a relation. *)
  val add : 'a t -> 'a -> 'a t

  (** Add a relation between two elements. *)
  val relate : 'a t -> 'a -> 'a -> 'a t
end
