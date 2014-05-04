(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * pagerank.ml -- performs pagerank operations on the elements, which is
 * personalized for each owner *)

open Core.Std
open Matrix.FloatMatrix

(* Returns an array of vectors, where each vector represents the pageranks of an
 * owner. pagerank = exponential of dot product matrix * owner's preference *)
let pageranks (matrix : mat) =
  let dot_products = mult_mat matrix (transpose matrix) in
  let maxes = Array.map ~f:(Array.fold ~f:Float.max ~init:0.) dot_products in
  let max = Array.fold ~f:Float.max ~init:0. maxes in
  let dot_products = scalar_mult_mat dot_products (1. /. max) in
  let mat_exponential = exponentiate dot_products in
  mult_mat mat_exponential matrix

(* Converts a matrix of pageranks into a matrix of costs, by subtracting each
 * value from the largest value in the matrix *)
let cost_matrix (matrix : mat) : mat =
  let f acc x = max x acc in
  let max_entry = Helpers.matrix_fold matrix ~f ~init:0. in
  let costs = Helpers.matrix_map matrix ~f:(fun x -> max_entry -. x) in
  costs
