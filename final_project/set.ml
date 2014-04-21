(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * set.ml -- provides implementation of sets/trees for use with dictionaries *)

module type SET =
sig
  (* types of values in set *)
  type value
  (* type for the set (which will be a ref) *)
  type set

  (* the empty set *)
  val empty : set

  (* adds value to set *)
  val add : set -> value -> unit

  (* checks if value is in set *)
  val check : set -> value -> bool

  (* removes a value from the set *)
  val remove : set -> value -> unit

  (* clears the set *)
  val clear : set -> unit

  (* reduces over the set *)
  val fold : (value -> 'a -> 'a) -> 'a -> set -> 'a
  
  (* returns string of value *)
  val string_of_value : value -> string
end
