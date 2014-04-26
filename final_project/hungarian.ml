(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * hungarian.ml -- Provides interface for implementation of the Hungarian
 * algorithm, which minimizes the cost of matching in a cost matrix. *)

open Core.Std
open Polynomial
open Matrix.FloatMatrix

exception Empty

Random.self_init ();

module type HUNGARIAN =
sig
  type value
  type vec
  type mat

  (* allows us to compensate for floating-point inprecision -- this allows us to
   * treat numbers so close to 0 as practically 0 *)
  val zero_sensitivity : float

  (* runs the Hungarian algorithm on the matrix, returning (int * int) tuples
   * that represent the matches of columns and rows *)
  val optimize : mat -> (int * int) option

  (* prints the results of running the Hungarian algorithm  *)
  val print_results : (int * int) list -> unit
end

(* NOTE: I have switched the meaning of columns and rows, relative to the
 * Wikipedia article (i.e. in this implementation, each column represents
 * a person and each row represents a choice, rather than vice versa). *)

let zero_sensitivity = 0.1;;





let add_to_vec (v : vec) (x : float) : vec = Array.map ~f:((+.) x) v

let zero_indices (v : vec) : int list =
  let l = ref [] in
  for i = Array.length v - 1 downto 0 do
    if Float.abs v.(i) <= zero_sensitivity then l := i :: !l
  done;
  !l

let find_min_vec (v : vec) : float =
  if Array.length v = 0 then raise Empty
  else Array.fold_right ~f:Float.min ~init:v.(0) v

let shift_to_zero (v : vec) : vec = add_to_vec v ((-1.) *. (find_min_vec v))

let swap_list : (int * int) list -> (int * int) list =
  let swap (x,y) = (y,x) in List.map ~f:swap

(* Checks the current matrix to see if an optimal assignment can be carried out,
 * after steps 1 and 2 of the algorithm have been done. *)
(* INCOMPLETE [see note at the bottom of the function] *)
let is_trivially_finished (m : mat) : (int * int) list option =
  let zeros : int list array = Array.map ~f:zero_indices m in
  if Array.fold_right ~f:(fun l c -> c || List.length l = 0) ~init:false zeros
  then None
  else
    let dim = Array.length m in
    let row_checker = ref true in
    (* this line is dumb, there must be some better way to do it, I just don't feel like finding it *)
    let zeros_rows = List.to_array (List.concat (Array.to_list zeros)) in
    for i = 0 to dim - 1 do
      if not (Array.mem zeros_rows i) then row_checker := false
    done;
    if not !row_checker then None
    else
      let result : (int * int) list ref = ref [] in
      let is_assigned ((person, element) : int * int) : bool =
	List.fold_left ~f:(fun c (x,y) -> c || x = person || y = element) ~init:false !result in
      let f i l =
	match l with
	| [j] -> if not (is_assigned (i,j)) then result := (i,j) :: !result
	| _ -> ()
      in Array.iteri ~f zeros;
      let zeros_trans : int list array = Array.map ~f:zero_indices (transpose m) in
      let f j l =
	match l with
	| [i] -> if not (is_assigned (i,j)) then result := (i,j) :: !result
        | _ -> ()
      in Array.iteri ~f zeros_trans;
      if List.length !result = dim then Some !result else None
      (* NOTE: THIS IS INCOMPLETE -- There are still cases where the function
       * returns None when there is a solution (specifically, the function
       * ignores rows/columns that have more than 1 zero, while those situations
       * could still have a solution). *)

(* Subtracts the lowest element from each row, and from each column *)
let steps_12 (m : mat) : mat =
  transpose (Array.map ~f:shift_to_zero (transpose (Array.map ~f:shift_to_zero m)))

let rec print_results (r : (int * int) list) : unit =
  (match r with
   | [] -> Printf.printf "\n\n"
   | (i,j) :: r' ->
      Printf.printf "Person %i assigned to element %i.\n" i j;
      print_results r');
  flush_all ()


(* Generates a random dim x dim matrix of integers from 0 to 99 *)
let random_matrix (dim : int) : mat =
  let m : mat ref = ref [||] in
  for i = 0 to dim - 1 do
    let v = Array.create ~len:dim 0. in
    for j = 0 to dim - 1 do
      v.(j) <- float (Random.int 100);
    done;
    m := Array.append !m [|v|]
  done;
  !m

(* Tests steps 1 and 2 of the algorithm (what we have so far) by randomly
 * generating a matrix, and testing the algorithm. If it yields a result,
 * stop; else try again with another random matrix until it works. *)
let rec test1 () : unit =
  let m = random_matrix 4 in
  print_mat m; Printf.printf "\n";
  let m' = steps_12 m in
  Printf.printf "After steps 1 and 2:\n"; print_mat m'; Printf.printf "\n";
  match is_trivially_finished m' with
  | None -> Printf.printf "Failed attempt.\n"; test1 ()
  | Some r -> print_results r

(* Test function that gives me a feel for how often steps 1 and 2 of the
 * algorithm are sufficient to solve the problem. *)
let test2 (num_tries : int) (dim : int) : unit =
  let counter = ref 0 in
  for i = 0 to num_tries - 1 do
    let m = random_matrix dim in
    if is_trivially_finished (steps_12 m) <> None then counter := !counter + 1
  done;
  Printf.printf "\n\n%i attempts (of %i total) led to a solution from steps 1 and 2, when working on %ix%i matrices.\n" !counter num_tries dim dim

in test1 (); test2 10000 4
