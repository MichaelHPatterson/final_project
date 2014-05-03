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

(* Converts a matrix of pageranks into a matrix of costs *)
let cost_matrix (matrix : mat) : mat = 
  let max_entry = Helpers.matrix_fold matrix ~f:(
    fun acc x -> max x acc) ~init:0. in
  let costs = Helpers.matrix_map matrix ~f:(fun x -> max_entry -. x) in
  costs

let owner_rank (ranks : mat) (owner_ind : int) (elt_ind : int) : int =
  let owner_vector = ranks.(owner_ind) in
  let my_val = owner_vector.(elt_ind) in
  let sorted_list = List.rev (List.sort ~cmp:Float.compare 
    (Array.to_list owner_vector)) in
  let rec find_ind lst my_val num =
    match lst with
    | x :: xs -> if compare x my_val=0 then num else find_ind xs my_val (num+1)
    | _ -> failwith "elt_index not found" in
  find_ind sorted_list my_val 1
  
let owners_ranks (input : (int * int) list) (ranks : mat) : int list =
  let fst_sort (x,_) (y,_) : int = Int.compare x y in
  List.fold_right (List.sort ~cmp:fst_sort input) ~f:(
    fun (x,y) acc -> (owner_rank ranks x y) :: acc)  ~init:[]

let hungarian_ranks (match_list : string list) (rank_list : int list) =
  List.map2_exn match_list rank_list ~f:(
    fun x y -> x ^ ", which had #" ^ (string_of_int y) ^ "ranking for this" ^
      " individual")

(* Generates a random dim x dim matrix of integers from 0 to 99 *)
let random_matrix (dim : int) : mat =
  let m : mat = zero_mat dim dim in
  for i = 0 to dim - 1 do
    let v = Array.create ~len:dim 0. in
    for j = 0 to dim - 1 do
      v.(j) <- float (Random.int 5);
    done;
    m.(i) <- v
  done;
  m

let tests (times : int) : unit =
  let total_time = ref 0. in
  for _i = 0 to times do
    let start_time = Unix.gettimeofday () in
    ignore (pageranks (random_matrix 20));
    let end_time = Unix.gettimeofday () in
    let time = end_time -. start_time in
    total_time := !total_time +. time;
    Printf.printf "\nDONE!!! That took a total of %f seconds.\n\n" time; flush_all ();
  done;
  Printf.printf "==============\nAVERAGE TIME: %f seconds\n\n" (!total_time /. (float times));;
