(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * pagerank.ml -- performs pagerank operations on the elements, which is
 * personalized for each owner *)

module type PAGERANK =
sig

  type dict

  type vec
  type mat

  (* performs final operations to the relationship matrix, e.g. adding some
   * multiple of the identity matrix *)
  val finalize : mat -> unit

  (* instantiates a vector of owner preferences *)
  val preferences : dict -> vec

  (* given a vector of owner preferences, returns the personalized pagerank,
   * defined as the vector * the matrix exponential of the relationship 
   * matrix *)
  val calculate : vec -> mat -> vec
end
