module type Hashable = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module type T = sig
  type key
  type 'a t

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> unit
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val remove : key -> 'a t -> bool
  val print : (key -> string) -> ('a -> string) -> 'a t -> string -> unit
  val depth : 'a t -> int
  val size : 'a t -> int
  val snapshot : 'a t -> 'a t
end

module Make : functor (H : Hashable) -> T with type key = H.t
