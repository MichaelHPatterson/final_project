(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * relationship.ml -- provides operations to return a matrix of relationship
 * values between elements *)

open Core.std

module type RELATIONSHIP =
sig
  type key
  type value
  type dict

  type mat

  (* takes the dicts of two elements' rankings and calculates the relationship
   * between them *)
  val calculate : dict -> dict -> float

  (* calculates the relationships of one element with the rest of the
   * elements *)
  val calc_column : dict -> dict list -> (string * float) list

  (* calculates the relationships of all elements with all of the rest,
   * loading them into a matrix *)
  val load_all : dict list -> mat
end
