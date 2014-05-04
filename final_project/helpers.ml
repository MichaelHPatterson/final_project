(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * helpers.ml -- misc. helper functions related to matrices *)

open Core.Std

(* general fold function for matrices *)
let matrix_fold (matrix : 'b array array) ~(f : 'a -> 'b -> 'a) ~(init : 'a)
    : 'a =
  Array.fold matrix ~init:init ~f:(fun acc x -> Array.fold x ~init:acc ~f:f)

(* general map function for matrices *)
let matrix_map (matrix : 'a array array) ~(f : 'a -> 'b) : 'b array array =
  let len = Array.length matrix in
  let result = Array.make_matrix ~dimx:len ~dimy:len (f (matrix.(0).(0))) in
  for i = 0 to len -1 do
    result.(i) <- Array.map ~f:f matrix.(i)
  done;
  result
