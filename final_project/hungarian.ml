(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * hungarian.ml -- Provides interface for implementation of the Hungarian
 * algorithm, which minimizes the cost of matching in a cost matrix. *)

open Core.Std
open Polynomial
open Matrix.FloatMatrix

exception Empty
exception AlgorithmError

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

let zero_sensitivity = 0.01;;





let add_to_vec (v : vec) (x : float) : vec = Array.map ~f:((+.) x) v

let is_zero (x : float) : bool = Float.abs x <= zero_sensitivity

let zero_indices (v : vec) : int list =
  let l = ref [] in
  for i = Array.length v - 1 downto 0 do
    if is_zero v.(i) then l := i :: !l
  done;
  !l

let find_min_vec (v : vec) : float =
  if Array.length v = 0 then raise Empty
  else Array.fold_right ~f:Float.min ~init:v.(0) v

let shift_to_zero (v : vec) : vec = add_to_vec v ((-1.) *. (find_min_vec v))

let swap_list : (int * int) list -> (int * int) list =
  let swap (x,y) = (y,x) in List.map ~f:swap

type hungarian_status = Finished of (int * int) list | Unfinished of (int * int) list

(* Checks the current matrix to see if an optimal assignment can be carried out,
 * after steps 1 and 2 of the algorithm have been done. *)
(* INCOMPLETE [see note at the bottom of the function] *)
let is_trivially_finished (m : mat) : hungarian_status =
  let zeros : int list array = Array.map ~f:zero_indices m in
  (*if Array.fold_right ~f:(fun l c -> c || List.length l = 0) ~init:false zeros
  then None
  else*)
    let dim = Array.length m in
    (*let row_checker = ref true in*)
    (* this line is dumb, there must be some better way to do it, I just don't feel like finding it *)
    (*let zeros_rows = List.to_array (List.concat (Array.to_list zeros)) in
    for i = 0 to dim - 1 do
      if not (Array.mem zeros_rows i) then row_checker := false
    done;
    if not !row_checker then None
    else*)
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
      if List.length !result = dim then Finished !result else Unfinished !result
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

(* Arguments: (1) the matrix, after applying steps_12, and (2) the assignments that have already been made,
 * in the form of column * row.
 * Returns: (1) List of "marked" columns. (2) List of "marked" rows. Note that these
 * "markings" denote the smallest number of lines required to cover all 0's in m. *)
let mark_zeros (m : mat) (assignments : (int * int) list) : int list * int list =
  let marked_cols : int list ref = ref [] in
  let marked_rows : int list ref = ref [] in
  let dim = Array.length m in
  (* Mark all columns having no assignments. *)
  for i = 0 to dim - 1 do
    if List.fold_left ~f:(fun b (c,_) -> b && c <> i) ~init:true assignments then
      marked_cols := i :: !marked_cols
  done;
  let do_loop () =
    (* Mark all rows having zeros in marked columns. *)
    let f1 (c : int) : unit =
      let f i x =
	if is_zero x && not (List.mem !marked_rows i)
	then marked_rows := i :: !marked_rows in
      Array.iteri ~f m.(c) in
    List.iter ~f:f1 !marked_cols;
    (* Mark all columns having assignments in marked rows. *)
    let f2 (r : int) : unit =
      let f i _ =
	if List.mem assignments (i,r) && not (List.mem !marked_cols i)
	then marked_cols := i :: !marked_cols in
      Array.iteri ~f (transpose m).(r) in
    List.iter ~f:f2 !marked_rows
  in
  let rec call_loop () : unit =
    let (curr_marked_rows, curr_marked_cols) = (!marked_rows, !marked_cols) in
    do_loop ();
    if curr_marked_rows = !marked_rows && curr_marked_cols = !marked_cols then ()
    else call_loop ()
  in
  call_loop ();
  let new_marked_cols = ref [] in
  for i = 0 to dim - 1 do
    if not (List.mem !marked_cols i) then new_marked_cols := i :: !new_marked_cols
  done;
  let f i = Printf.printf " %i\n" i in
  Printf.printf "Marked columns:\n"; List.iter ~f !new_marked_cols;
  Printf.printf "\nMarked rows:\n"; List.iter ~f !marked_rows;
  (!new_marked_cols, !marked_rows)
      

(* Carries out steps 3 and 4 of the Hungarian algorithm, assuming that the
 * parameter matrix m has already undergone steps 1 and 2. *)
let rec steps_34 (m : mat) (assignments : (int * int) list) : (int * int) list =
  let dim = Array.length m in
  if List.length assignments = dim then assignments
  else
    (* This is inefficient -- the mark_zeros function gets called separately on each recursive call *)
    let (marked_cols, marked_rows) = mark_zeros m assignments in
    let unmarked = Array.map ~f:(Array.filteri ~f:(fun r _ -> not (List.mem marked_rows r))) (Array.filteri ~f:(fun c _ -> not (List.mem marked_cols c)) m) in
    (* This structure here is terrible -- figure out a better way to check whether the matrix is assignable without risking an error *)
    if Array.length unmarked = 0 || Array.length unmarked.(0) = 0 then
      match is_trivially_finished m with
      | Finished lst -> lst
      | Unfinished _ -> raise AlgorithmError
    else
      let min (c : vec) : float = Array.fold_right ~f:Float.min ~init:c.(0) c in
      let min_unmarked = Array.fold_right ~f:(fun col init -> Float.min (min col) init) ~init:unmarked.(0).(0) unmarked in
      Printf.printf "Minimum unmarked value: %f\n" min_unmarked;
      let is_double_marked c r = List.mem marked_cols c && List.mem marked_rows r in
      let is_unmarked c r = not (List.mem marked_cols c || List.mem marked_rows r) in
      let _ = try
	  (for c = 0 to dim - 1 do
	     for r = 0 to dim - 1 do
	       if is_double_marked c r then m.(c).(r) <- m.(c).(r) +. min_unmarked
	       else if is_unmarked c r then m.(c).(r) <- m.(c).(r) -. min_unmarked
	     done
	   done)
	with (Invalid_argument "index out of bounds") -> raise Matrix.NotSquare
      in
      print_mat m; Printf.printf "\n\n";
      match is_trivially_finished m with
      | Finished lst -> lst
      | Unfinished lst -> steps_34 m lst

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
  | Unfinished _ -> Printf.printf "Failed attempt.\n"; test1 ()
  | Finished r -> print_results r

(* Test function that gives a feel for how often steps 1 and 2 of the
 * algorithm are sufficient to solve the problem. *)
let test2 (num_tries : int) (dim : int) : unit =
  let counter = ref 0 in
  for i = 0 to num_tries - 1 do
    let m = steps_12 (random_matrix dim) in
    print_mat m; Printf.printf "\n";
    let solution = is_trivially_finished m in
    (match solution with
    | Finished assignments -> print_results assignments; counter := !counter + 1
    | Unfinished assignments ->
      print_results assignments;
      let assignments = steps_34 m assignments in
      Printf.printf "ASSIGNMENTS:\n"; print_results assignments);
    Printf.printf "\n\n\n";
  done;
  Printf.printf "\n\n%i attempts (of %i total) led to a solution from steps 1 and 2, when working on %ix%i matrices.\n" !counter num_tries dim dim

in test1 (); test2 1000 4
