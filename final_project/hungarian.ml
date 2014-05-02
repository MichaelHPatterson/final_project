(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * hungarian.ml -- Provides interface for implementation of the Hungarian
 * algorithm, which minimizes the cost of matching in a cost matrix. *)

open Core.Std
open Polynomial
open Matrix.FloatMatrix
open Read_write.FloatRead

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

let rec print_results (r : (int * int) list) : unit =
  (match r with
   | [] -> Printf.printf "\n\n"
   | (i,j) :: r' ->
      Printf.printf "Person %i assigned to element %i.\n" i j;
      print_results r');
  flush_all ()

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
let is_finished (m : mat) : hungarian_status =
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
      let is_assigned ((person, element) : int * int) (results : (int * int) list) : bool =
	(* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: I'm not sure if this line works -- have to verify *)
	List.exists ~f:(fun (p,e) -> p = person || e = element ) results in
	(* List.fold_left ~f:(fun c (p,e) -> c || p = person || e = element) ~init:false results in *)
      let f i l =
	match l with
	| [j] -> if not (is_assigned (i,j) !result) then result := (i,j) :: !result
	| _ -> ()
      in Array.iteri ~f zeros;
      let zeros_trans : int list array = Array.map ~f:zero_indices (transpose m) in
      let f j l =
	match l with
	| [i] -> if not (is_assigned (i,j) !result) then result := (i,j) :: !result
        | _ -> ()
      in Array.iteri ~f zeros_trans;
      match Int.compare (List.length !result) dim with
      | 1 -> raise AlgorithmError
      | 0 -> Finished !result
      | _ ->
	let unassigned_people = ref [] in
	let unassigned_elts = ref [] in
	for i = 0 to dim - 1 do
	  (* The 0's are just used as placeholders -- any integer would do just as well *)
	  if not (List.exists ~f:(fun (p,_) -> p = i) !result)
	  then unassigned_people := i :: !unassigned_people;
	  if not (List.exists ~f:(fun (_,e) -> e = i) !result)
	  then unassigned_elts := i :: !unassigned_elts;
	  flush_all ()
	done;
	let possible_new = List.cartesian_product !unassigned_people !unassigned_elts in
        let possible_new = List.filter ~f:(fun (p,e) -> not (is_assigned (p,e) !result) && is_zero m.(p).(e)) possible_new in
	let best_possible = ref !result in
	let rec check_finished (possible_pairs : (int * int) list) (curr_results : (int * int) list) : hungarian_status =
	  match possible_pairs with
	  | [] ->
	    (match Int.compare (List.length curr_results) dim with
	    | 1 -> raise AlgorithmError
	    | 0 -> Finished curr_results
	    | _ -> Unfinished curr_results)
	  | (p,e) :: possible_pairs' ->
	    if is_assigned (p,e) curr_results then check_finished possible_pairs' curr_results
	    else
	      (match check_finished possible_pairs' ((p,e) :: curr_results) with
	      | Unfinished r ->
		if List.length r > List.length !best_possible then best_possible := r;
		check_finished possible_pairs' curr_results
	      | Finished r -> Finished r)
	in
	match check_finished possible_new !result with
	| Finished r -> Finished r
	| Unfinished _ -> Unfinished !best_possible
	(*let f (p,e) =
	  if not (is_assigned (p,e) !result) && is_zero m.(p).(e)
	  then result := (p,e) :: !result
	in List.iter ~f !possible_new;
	if List.length !result = dim then Finished !result else	Unfinished !result*)
      (* NOTE: THIS IS INCOMPLETE -- There are still cases where the function
       * returns None when there is a solution (specifically, the function
       * ignores rows/columns that have more than 1 zero, while those situations
       * could still have a solution). *)

(* Subtracts the lowest element from each row, and from each column *)
let steps_12 (m : mat) : mat =
  transpose (Array.map ~f:shift_to_zero (transpose (Array.map ~f:shift_to_zero m)))

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
  (!new_marked_cols, !marked_rows)
      

(* Carries out steps 3 and 4 of the Hungarian algorithm, assuming that the
 * parameter matrix m has already undergone steps 1 and 2. *)
let rec steps_34 (m : mat) (assignments : (int * int) list) : (int * int) list =
  let dim = Array.length m in
  if List.length assignments = dim then assignments
  else
    (* This is inefficient -- the mark_zeros function gets called separately on each recursive call *)
    (*let _ = (print_mat m; Printf.printf "\n\n"; flush_all ()) in*)
    let (marked_cols, marked_rows) = mark_zeros m assignments in
    let unmarked = Array.map ~f:(Array.filteri ~f:(fun r _ -> not (List.mem marked_rows r))) (Array.filteri ~f:(fun c _ -> not (List.mem marked_cols c)) m) in
    (*********** This structure here is terrible -- figure out a better way to check whether the matrix is assignable without risking an error, and without having the dummy line "raise AlgorithmError", if possible ***********)
    (**************************** THIS SOMETIMES CAUSES ALGORITHMERROR TO GET RAISED ****************************)
    if Array.length unmarked = 0 || Array.length unmarked.(0) = 0 then
      match is_finished m with
      | Finished lst -> lst
      | Unfinished lst -> (print_mat m;
			   Printf.printf "Marked columns:"; List.iter ~f:(fun x -> Printf.printf " %i" x) marked_cols;
			   Printf.printf "\nMarked rows:"; List.iter ~f:(fun x -> Printf.printf " %i" x) marked_rows;
			   Printf.printf "\n";
			   print_results lst;
			   raise AlgorithmError)
    else
      let min (c : vec) : float = Array.fold_right ~f:Float.min ~init:c.(0) c in
      let min_unmarked = Array.fold_right ~f:(fun col init -> Float.min (min col) init) ~init:unmarked.(0).(0) unmarked in
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
      match is_finished m with
      | Finished lst -> lst
      | Unfinished lst -> steps_34 m lst

let hungarian_test (m : mat) : unit =
  print_mat m;
  let m = steps_12 m in
  match is_finished m with
  | Finished results -> print_results results
  | Unfinished results ->
    let results = steps_34 m results in
    print_results results

(* shitty function for testing *)
let brute_force (m : mat) : float =
  let len = Array.length m in
  let generate_perm () : int array =
    let perm = Array.create ~len (-1) in
    for i = 0 to len - 1 do
      let rec gen_num () : unit =
	let new_num = Random.int len in
	if Array.mem perm new_num then gen_num ()
	else perm.(i) <- new_num
      in gen_num ()
    done;
    (* NOTE: This screws up if the size of the matrix is 0... but it's just a shitty test function anyway *)
    Printf.printf "[|%i" perm.(0);
    for i = 1 to len - 1 do
      Printf.printf "; %i" perm.(i)
    done;
    Printf.printf "|]";
    flush_all ();
    perm
  in
  let perm = generate_perm () in
  let cost = ref 0. in
  for i = 0 to len - 1 do
    cost := !cost +. m.(i).(perm.(i))
  done;
  !cost

let brute_force_n (m : mat) (n : int) : unit =
  for _i = 0 to n - 1 do
    let cost = brute_force m in
    Printf.printf "\t=====>\t%f\n" cost;
    flush_all ()
  done

(* Generates a random dim x dim matrix of integers from 0 to 99 *)
let random_matrix (dim : int) : mat =
  let m : mat = zero_mat dim dim in
  for i = 0 to dim - 1 do
    let v = Array.create ~len:dim 0. in
    for j = 0 to dim - 1 do
      v.(j) <- float (Random.int 100);
    done;
    m.(i) <- v
  done;
  m

(* formats the results of the Hungarian algorithm in terms of string pairs,
 * assuming that the argument is (owner index * elt index) pairs *)
let format_hungarian (lst : (int * int) list) (owner_dict : dict)
  (elt_dict : dict) : (string * string) list =
  let dict_list (d : dict) : ((string * int) list) = dict_fold (
    fun k v acc -> ((string_of_key k),(int_of_val v)) :: acc) [] d in
  let fst_sort a b : int =
    let (x, _) = a in
    let (y, _) = b in
    Int.compare x y in
  let snd_sort a b : int =
    let (_, x) = a in
    let (_, y) = b in
    Int.compare x y in
  let elt_sorted = List.sort ~cmp:snd_sort (dict_list elt_dict) in
  let owner_sorted = List.sort ~cmp:snd_sort (dict_list owner_dict) in
  let snd_results_sorted = List.sort ~cmp:snd_sort lst in
  let elt_strings = List.map2_exn elt_sorted snd_results_sorted ~f:(
    fun (x,_) (z,_) -> (z,x)) in
  let fst_results_sorted = List.sort ~cmp:fst_sort elt_strings in
  let add_owner_strings = List.map2_exn owner_sorted fst_results_sorted ~f:(
    fun (x,_) (_,a) -> (x,a)) in
  add_owner_strings
  
(* expected results for our float_write_test *)
let float_write_results = [("Bob", "Tobacco"); ("Janet", "Alcohol"); 
    ("Morgan", "LSD"); ("Po", "Weed"); ("Dan", "Heroin")]

let format_test (test_ints : ((int * int) list))
  (expected_results : ((string * string) list)) (hung_owners : dict)
  (hung_elts : dict) : unit =
  let my_results = format_hungarian test_ints hung_owners hung_elts in
  let print_results (my_list : (string * string) list) : unit =
    List.fold_left my_list ~init:() ~f:(fun _ (x,y) -> 
      Out_channel.output_string stdout (x ^ "  " ^ y ^ "\n"); flush_all()) in
  if (expected_results = my_results) then 
    Out_channel.output_string stdout "Success" else print_results my_results

(* Tests steps 1 and 2 of the algorithm (what we have so far) by randomly
 * generating a matrix, and testing the algorithm. If it yields a result,
 * stop; else try again with another random matrix until it works. *)
let rec test1 () : unit =
  let m = random_matrix 4 in
  print_mat m; Printf.printf "\n";
  let m' = steps_12 m in
  Printf.printf "After steps 1 and 2:\n"; print_mat m'; Printf.printf "\n";
  (match is_finished m' with
  | Unfinished _ -> Printf.printf "Failed attempt.\n"; test1 ()
  | Finished r -> print_results r);
  flush_all ()

let my_matrix = [| [| 586.; 12909.; 3164.; 8271.; 16741. |]; 
                   [| 8421.; 17309.; 10279.; 13950.; 20069. |]; 
                   [| 10386.; 18385.; 12054.; 15384.; 20891. |];
                   [| 0.; 12578.; 2628.; 7845.; 16494. |];
                   [| 6742.; 16370.; 8761.; 12743.; 19361. |] |] in
hungarian_test my_matrix;
Printf.printf "\n"


(* Test function that gives a feel for how often steps 1 and 2 of the
 * algorithm are sufficient to solve the problem. *)
let test2 (dim : int) (num_tries : int) : unit =
  let counter = ref 0 in
  let total_time = ref 0. in
  for _i = 0 to num_tries - 1 do
    (* NOTE: This "gettimeofday" function will give weird values if the program
   is run at 11:59:59, since it resets
     * to zero at 00:00:00.*)
    let start_time = Unix.gettimeofday () in
    let m = random_matrix dim in
    let m = steps_12 m in
    let solution = is_finished m in
    (match solution with
    | Finished _ -> (counter := !counter + 1)
    | Unfinished assignments -> ignore (steps_34 m assignments));
    let end_time = Unix.gettimeofday () in
    total_time := !total_time +. end_time -. start_time
  done;
  let avg_time = !total_time /. (float num_tries) in
  Printf.printf "%i attempts (of %i total) led to a solution from steps 1 and 2 alone, when working on %ix%i matrices.\n" !counter num_tries dim dim;
  Printf.printf "On average, each test took %f, " avg_time;
  Printf.printf "or (%i * %f)^3, seconds.\n" dim (avg_time ** (1. /. 3.) /. (float dim));
  flush_all ()

in test1 (); test2 4 1000; test2 5 1000; test2 6 1000; test2 7 1000; test2 8 1000; test2 9 1000; test2 10 1000; test2 12 1000; test2 15 100; test2 18 100; test2 20 100; test2 30 1; test2 40 1; test2 50 1; test2 75 1; test2 90 1; test2 100 1; test2 250 1
