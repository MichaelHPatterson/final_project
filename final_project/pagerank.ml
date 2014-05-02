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
  Out_channel.output_string stdout "hi I'm here haaands \n";
  Printf.printf "ORIGINAL MATRIX:\n";
  print_mat dot_products;
  Printf.printf "===================";
  flush_all();
  let mat_exponential = exponentiate dot_products in
  print_mat mat_exponential;
  flush_all();
  mult_mat mat_exponential matrix

(* Generates a random dim x dim matrix of integers from 0 to 99 *)
let random_matrix (dim : int) : mat =
  let m : mat = zero_mat dim dim in
  for i = 0 to dim - 1 do
    let v = Array.create ~len:dim 0. in
    for j = 0 to dim - 1 do
      v.(j) <- float (Random.int 10);
    done;
    m.(i) <- v
  done;
  m

let tests (times : int) : unit =
  for _i = 0 to times do
    ignore (pageranks (random_matrix 3))
  done;;

tests 50;;
