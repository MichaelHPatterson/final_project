(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * hungarian.ml -- Provides interface for implementation of the Hungarian
 * algorithm, which minimizes the cost of matching in a cost matrix. *)

open Core.Std
open Matrix.FloatMatrix
open Psetdict.StringIntDict

exception Empty
exception AlgorithmError

Random.self_init ();;

(* Represents how sensitive the program is about values close to zero. A value
 * that is less than zero_sensitivity is considered equal to 0. *)
let zero_sensitivity = 0.01;;


(*******************************HELPER FUNCTIONS*******************************)

(* Adds a scalar value to every element in a vector. *)
let add_to_vec (v : vec) (x : float) : vec = Array.map ~f:((+.) x) v

(* Checks whether a floating point value is "close enough" to zero. *)
let is_zero (x : float) : bool = Float.abs x <= zero_sensitivity

let rec print_results (r : (int * int) list) : unit =
  (match r with
   | [] -> Printf.printf "\n\n"
   | (i,j) :: r' ->
      Printf.printf "Person %i assigned to element %i.\n" i j;
      print_results r');
  flush_all ()

(* Returns a list containing the indices of all zeros in v. *)
let zero_indices (v : vec) : int list =
  let l = ref [] in
  for i = Array.length v - 1 downto 0 do
    if is_zero v.(i) then l := i :: !l
  done;
  !l

(* Finds the smallest element in v. *)
let find_min_vec (v : vec) : float =
  if Array.length v = 0 then raise Empty
  else Array.fold_right ~f:Float.min ~init:v.(0) v

(* Subtracts the smallest element of v from each element, so that its lowest
 * element is zero and all other elements are nonnegative. *)
let shift_to_zero (v : vec) : vec = add_to_vec v ((-1.) *. (find_min_vec v))



(********************************CORE ALGORITHM********************************)

(* Subtracts the lowest element from each row, and from each column. Called
 * "steps_12" because these actions are described as steps 1 and 2 of the
 * algorithm on Wikipedia. *)
let steps_12 (m : mat) : mat =
  let m = Array.map ~f:shift_to_zero m in
  transpose (Array.map ~f:shift_to_zero (transpose m))

(* A type used below to check whether ALL assignments can be made (Finished), or
 * that's not yet possible (Unfinished). *)
type hungarian_status = Finished of (int * int) list
		      | Unfinished of (int * int) list

(* Checks the current matrix to see if an optimal assignment can be carried out
 * based on the locations of 0's in m, after applying steps_12. *)
let is_finished (m : mat) : hungarian_status =
  let zeros : int list array = Array.map ~f:zero_indices m in
  let dim = Array.length m in
  let result : (int * int) list ref = ref [] in
  let is_assigned ((a,b) : int * int) (results : (int * int) list) : bool =
    List.exists ~f:(fun (a',b') -> a' = a || b' = b ) results in
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
      if not (List.exists ~f:(fun (a,_) -> a = i) !result)
      then unassigned_people := i :: !unassigned_people;
      if not (List.exists ~f:(fun (_,b) -> b = i) !result)
      then unassigned_elts := i :: !unassigned_elts;
    done;
    let poss_new = List.cartesian_product !unassigned_people !unassigned_elts in
    let f (a,b) = not (is_assigned (a,b) !result) && is_zero m.(a).(b) in
    let poss_new = List.filter ~f poss_new in
    let best_possible = ref !result in
    let rec check_finished (possible : (int * int) list)
			   (curr : (int * int) list) : hungarian_status =
      match possible with
      | [] ->
	 (match Int.compare (List.length curr) dim with
	  | 1 -> raise AlgorithmError
	  | 0 -> Finished curr
	  | _ -> Unfinished curr)
      | (p,e) :: possible' ->
	 if is_assigned (p,e) curr then check_finished possible' curr
	 else
	   (match check_finished possible' ((p,e) :: curr) with
	    | Unfinished r ->
	       if List.length r > List.length !best_possible then
		 best_possible := r;
	       check_finished possible' curr
	    | Finished r -> Finished r)
    in
    match check_finished poss_new !result with
    | Finished r -> Finished r
    | Unfinished _ -> Unfinished !best_possible

(* "Marks" the columns and rows of m, returning the smallest set of markings
 * required to cover all zeros in m. Takes the matrix m and the assignments made
 * so far as arguments (which is the list "curr"). *)
let mark_zeros (m : mat) (curr : (int * int) list) : int list * int list =
  let unmarked_cols : int list ref = ref [] in
  let marked_rows : int list ref = ref [] in
  let dim = Array.length m in
  (* Mark all columns having no assignments. *)
  for i = 0 to dim - 1 do
    if List.fold_left ~f:(fun b (c,_) -> b && c <> i) ~init:true curr then
      unmarked_cols := i :: !unmarked_cols
  done;
  let do_loop () =
    (* Mark all rows having zeros in marked columns. *)
    let f1 (c : int) : unit =
      let f i x =
	if is_zero x && not (List.mem !marked_rows i)
	then marked_rows := i :: !marked_rows in
      Array.iteri ~f m.(c) in
    List.iter ~f:f1 !unmarked_cols;
    (* Mark all columns having assignments in marked rows. *)
    let f2 (r : int) : unit =
      let f i _ =
	if List.mem curr (i,r) && not (List.mem !unmarked_cols i)
	then unmarked_cols := i :: !unmarked_cols in
      Array.iteri ~f (transpose m).(r) in
    List.iter ~f:f2 !marked_rows
  in
  let rec call_loop () : unit =
    let (curr_marked_rows, curr_marked_cols) = (!marked_rows, !unmarked_cols) in
    do_loop ();
    if curr_marked_rows = !marked_rows && curr_marked_cols = !unmarked_cols
    then ()
    else call_loop ()
  in
  call_loop ();
  let marked_cols = ref [] in
  for i = 0 to dim - 1 do
    if not (List.mem !unmarked_cols i) then marked_cols := i :: !marked_cols
  done;
  (!marked_cols, !marked_rows)
      
(* Carries out steps 3 and 4 of the Hungarian algorithm, assuming that the
 * parameter matrix m has already undergone steps 1 and 2. Subtracts the
 * smallest unmarked value from all unmarked values and adds it to all double-
 * marked values, and repeats that process until an assignment can be done. *)
let rec steps_34 (m : mat) (assignments : (int * int) list) : (int * int) list =
  let dim = Array.length m in
  if List.length assignments = dim then assignments
  else
    let (marked_cols, marked_rows) = mark_zeros m assignments in
    let f c _ = not (List.mem marked_cols c) in
    let unmarked = Array.filteri ~f m in
    let f = Array.filteri ~f:(fun r _ -> not (List.mem marked_rows r)) in
    let unmarked = Array.map ~f unmarked in
    if Array.length unmarked = 0 || Array.length unmarked.(0) = 0 then
      match is_finished m with
      | Finished lst -> lst
      | Unfinished _ ->
	 (* This branch should never be reached; if it is, something's wrong *)
	 raise AlgorithmError
    else
      let min (c : vec) : float = Array.fold_right ~f:Float.min ~init:c.(0) c in
      let f col init = Float.min (min col) init in
      let min_unmarked = Array.fold_right ~f ~init:unmarked.(0).(0) unmarked in
      let is_double_marked c r =
	List.mem marked_cols c && List.mem marked_rows r in
      let is_unmarked c r =
	not (List.mem marked_cols c || List.mem marked_rows r) in
      let _ = try
	  (for c = 0 to dim - 1 do
	     for r = 0 to dim - 1 do
	       if is_double_marked c r then
		 m.(c).(r) <- m.(c).(r) +. min_unmarked
	       else if is_unmarked c r then
		 m.(c).(r) <- m.(c).(r) -. min_unmarked
	     done
	   done)
	with (Invalid_argument "index out of bounds") -> raise Matrix.NotSquare
      in
      match is_finished m with
      | Finished lst -> lst
      | Unfinished lst -> steps_34 m lst



(****************************FUNCTIONS FOR TESTING*****************************)

(* Tests the algorithm by printing a matrix, carrying out the algorithm, and
 * printing out the results. *)
let hungarian_test (m : mat) : unit =
  print_mat m;
  let m = steps_12 m in
  match is_finished m with
  | Finished results -> print_results results
  | Unfinished results ->
    let results = steps_34 m results in
    print_results results

(* Generates a random set of assignments, and return the cost of that random
 * permutation. Helps for testing optimality. *)
let brute_force (m : mat) : float =
  let len = Array.length m in
  if len = 0 then 0.
  else
    let perm = Array.create ~len (-1) in
    let cost = ref 0. in
    for i = 0 to len - 1 do
      let rec gen_num () : unit =
	let new_num = Random.int len in
	if Array.mem perm new_num then gen_num ()
	else perm.(i) <- new_num
      in gen_num ();
	 cost := !cost +. m.(i).(perm.(i))
    done;
    !cost

(* Carries out brute_force n times and asserts that each randomly generated
 * assignment has cost greater than or equal to the lowest cost inputted. *)
let brute_force_n (m : mat) (n : int) (lowest_cost : float) : unit =
  let costs = ref [] in
  for _i = 0 to n - 1 do
    let cost = brute_force m in
    costs := cost :: !costs
  done;
  List.iter ~f:(fun c -> assert (c >= lowest_cost)) !costs

(* Generates a random dim x dim matrix of integers (casted as floats) from 0 to
 * 99, inclusive. *)
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

(* Carries out all steps of the algorithm. *)
let hungarian (m : mat) : (int * int) list =
  print_mat m; Printf.printf "\n";
  let time1 = Unix.gettimeofday () in
  let m = steps_12 m in
  let time2 = Unix.gettimeofday () in
  Printf.printf "Steps 1 and 2 took %f seconds.\n" (time2 -. time1);
  match is_finished m with
  | Finished lst -> (let time3 = Unix.gettimeofday () in Printf.printf "Finished using just steps 1 and 2. Checking is_finished took %f seconds.\n" (time3 -. time2); lst)
  | Unfinished lst -> (print_mat m; Printf.printf "\n"; print_results lst; let time3 = Unix.gettimeofday () in let x = steps_34 m lst in let time4 = Unix.gettimeofday () in Printf.printf "Proceeded to steps 3 and 4. Checking is_finished took %f seconds, while steps 3 and 4 took %f seconds.\n" (time3 -. time2) (time4 -. time3); x)
  
(* formats the results of the Hungarian algorithm in terms of string pairs,
 * assuming that the argument is (owner index * elt index) pairs *)
let format_hungarian (lst : (int * int) list) (owner_dict : dict)
  (elt_dict : dict) : string list =
  let convert_to_string = string_of_key in
  let convert_to_int = int_of_val in
  let dict_list (d : dict) : ((string * int) list) = fold (
    fun k v acc -> ((convert_to_string k),(convert_to_int v)) :: acc) [] d in
  let fst_sort (x,_) (y,_) : int = Int.compare x y in
  let snd_sort (_,x) (_,y) : int = Int.compare x y in
  let elt_sorted = List.sort ~cmp:snd_sort (dict_list elt_dict) in
  let owner_sorted = List.sort ~cmp:snd_sort (dict_list owner_dict) in
  let snd_results_sorted = List.sort ~cmp:snd_sort lst in
  let elt_strings = List.map2_exn elt_sorted snd_results_sorted ~f:(
    fun (x,_) (z,_) -> (z,x)) in
  let fst_results_sorted = List.sort ~cmp:fst_sort elt_strings in
  let add_owner_strings = List.map2_exn owner_sorted fst_results_sorted ~f:(
    fun (x,_) (_,a) -> (x,a)) in
  let string_format (s1 : string) (s2 : string) : string =
	s1 ^ " matched with " ^ s2 in
  List.map add_owner_strings ~f:(fun (x,y) -> string_format x y)

(* expected results for our float_write_test *)
let float_write_results = [("Bob", "Tobacco"); ("Janet", "Alcohol"); 
    ("Morgan", "LSD"); ("Po", "Weed"); ("Dan", "Heroin")]
(*
let format_test (test_ints : ((int * int) list))
  (expected_results : ((string * string) list)) (hung_owners : dict)
  (hung_elts : dict) : unit =
  let my_results = format_hungarian test_ints hung_owners hung_elts in
  let print_results (my_list : (string * string) list) : unit =
    List.fold_left my_list ~init:() ~f:(fun _ (x,y) -> 
      Out_channel.output_string stdout (x ^ "  " ^ y ^ "\n"); flush_all()) in
  if (expected_results = my_results) then 
    Out_channel.output_string stdout "Success" else print_results my_results
 *)
(* Tests steps 1 and 2 of the algorithm (what we have so far) by randomly
 * generating a matrix, and testing the algorithm. If it yields a result,
 * stop; else try again with another random matrix until it works. *)
let rec test1 () : unit =
  let m = random_matrix 4 in
  print_mat m; Printf.printf "\n";
  let m' = steps_12 m in
  (match is_finished m' with
  | Unfinished _ -> Printf.printf "Failed attempt.\n"; test1 ()
  | Finished r -> print_results r);
  flush_all ()


(* Test function that gives a feel for runtimes and optimality. *)
let test2 (dim : int) (num_tries : int) : unit =
  let total_time = ref 0. in
  for _i = 0 to num_tries - 1 do
    let start_time = Unix.gettimeofday () in
    let matrix = random_matrix dim in
    let m = steps_12 matrix in
    let solution = is_finished m in
    let pairs = (match solution with
    | Finished assignments -> assignments
    | Unfinished assignments -> steps_34 m assignments) in
    let end_time = Unix.gettimeofday () in
    total_time := !total_time +. end_time -. start_time;
    let f c (a,b) = c +. matrix.(a).(b) in
    let cost = List.fold_left ~f ~init:0. pairs in
    brute_force_n matrix 100 cost
  done;
  let avg_time = !total_time /. (float num_tries) in
  Printf.printf "On average, for %ix%i matrices, each test " dim dim;
  Printf.printf "took %f seconds.\n" avg_time;
  flush_all ()

let run_tests () =
  test1 (); test2 4 1000; test2 5 1000; test2 6 1000; test2 7 1000; test2 8 1000; test2 9 1000; test2 10 1000; test2 12 1000; test2 15 100; test2 18 100; test2 20 100; test2 30 1; test2 40 1; test2 50 1; test2 75 1; test2 90 1; test2 100 1; test2 250 1
