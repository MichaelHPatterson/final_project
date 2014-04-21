(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * dict.ml -- provides an implementation of dictionaries, specifically an
 * AssocListDict and a Binary Search Tree (BST) Dict *)

open Core.std

module type DICT =
sig
  (* type of the keys of the dict, which will be searched through *)
  type key
  (* type of the values corresponding to keys *)
  type value
  (* type of the dict data structure (which will be a ref) *)
  type dict

  (* the empty dictionary *)
  val empty : dict

  (* adds (key, value) combo to the dict *)
  val add: dict -> (key, value) -> unit

  (* checks the value of a specific key; returns None if key is not in dict *)
  val check : dict -> key -> value option

  (* remove a (key, value) pair from the dict *)
  val remove : dict -> key -> unit

  (* clears the dict *)
  val clear : dict -> unit

  (* reduces the dictionary *)
  val fold : ((key, value) -> 'a -> 'a) -> 'a -> dict -> 'a

  (* returns string of key and value, respectively *)
  val string_of_key : key -> string
  val string_of_val : value -> string
end
  
