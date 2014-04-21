(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * ranking.ml -- provides the interface for the kind of ranking to be used in
 * the ranking of elements by owners and provides a means for comparison to be
 * used in relationship calculation *)

open Core.std

module type RANKING =
sig
  (* the kind of rank to be used *)
  type rank
 
  (* compares the first rank with the second *) 
  val compare : rank -> rank -> Ordering.t

  (* a float implementation of the rank *)
  val float_of_rank : rank -> float

  (* a string implementation of the rank *)
  val string_of_rank : rank -> string
end
