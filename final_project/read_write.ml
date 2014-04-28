(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .txt files *)

open Core.Std
open Psetdict

module type MATRIX =
sig
  (* type for matrix implementation *)
  type value

  val string_of_val : value -> string

  val big_test_matrix : value array array

end

(* Signature for writing to file. Note that the only exposed function is actually
 * writing to a file. This provides a favorable abstraction layer. *)

module type WRITE =
sig
  (* types for the matrix implementation *)
  type value
  type vec = value array
  type mat = vec array

  (* writes a whole matrix to a .txt file of given name *)
  val mat_to_file : mat -> string -> unit

  (* runs tests for writing *)
  val run_tests : unit -> unit

end


(* Signature for reading from a file. Note that the only exposed function is
 * actually reading the file, favorable for abstraction. *)

module type READ =
sig
  (* types for dict implementation *)
  type key
  type value
  type dict

  (* types for the matrix implementation *)
  type mat_value
  type vec
  type mat

  (* reads a .txt file of a specific invariant, returning the ranking matrix *)
  val process_file : string -> mat

  (* runs tests for writing *)
  (* val run_tests : unit -> unit *)

end
 
module IntMatrix : MATRIX =
struct
  type value = int

  let string_of_val = string_of_int

  let big_test_matrix = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] ; [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |]
end

module StringMatrix : MATRIX =
struct
  type value = string

  let string_of_val x = x

  let big_test_matrix = [| [| "Po"; "Doc"; "Pinky"; "Miles" |]; [| "A"; "B"; "C"; "D" |]; [| "1"; "2"; "3"; "4" |];
			   [| "a"; "b"; "c"; "d" |] |]
end


(* Module that writes matrices to a text file. Note that, in its current implementation,
 * this can only be used with int matrices. However, it could, and should, be made
 * abstract. That will come soon. Also, forgive the poor style; lots of lines are
 * longer than 80 chars *)

(* TODO for Write:
   -- Make this abstract. Use a functor for "value" and corresponding functions.
   -- Prepare for user input to be implemented in the near future.
   -- Prepare for labelling of matrix cols/rows with owners, elements
   -- Fix style
   -- Run more extensive tests
*)

module IntWrite(M: MATRIX) : (WRITE with type value = M.value) =
struct
  type value = M.value
  type vec = value array
  type mat = vec array

  (* general fold function for matrices; probably should go in matrix.ml *)
  let matrix_fold (matrix : 'b array array) ~(f : 'a -> 'b -> 'a) ~(init : 'a) : 'a =
    Array.fold matrix ~init:init ~f:(fun acc x -> Array.fold x ~init:acc ~f:f)

  (* general map function for matrices; probably should go in matrix.ml *)
  let matrix_map matrix f =
    let len = Array.length matrix in
    let result = Array.create ~len:len (Array.create ~len:len (f (matrix.(0)).(0))) in
    for i = 0 to len - 1 do
      result.(i) <- Array.map f matrix.(i)
    done;
    result

  (* formats a row of a string matrix for display in .txt file *)
  let row_concat (row : string array) (space : int) : string =
    let build_string (str : string) : string =
      (* space maker *)
      let rec make_space (empties : int) : string =
	if empties < 0 then "" else
	match empties with
	| 0 -> ""
	| _ -> " " ^ make_space (empties - 1) in
      
      (make_space (space - (String.length str))) ^ str in

    Array.fold_right row ~f:(fun x acc -> (build_string x) ^ acc) ~init:""

  (* calls row_concat to format the whole string matrix for display *)
  let string_mat_to_strings (matrix : string array array) (space : int) : string list  =
    Array.fold_right matrix ~f:(fun x acc -> (row_concat x space) :: acc) ~init:[]

  (* actually writes to file by converting to string matrix, formatting, and writing *)
  let mat_to_file (matrix : mat) (filename : string) : unit =
    let string_matrix = matrix_map matrix M.string_of_val in
    let max_len = matrix_fold string_matrix ~f:(fun acc x -> max (String.length x) acc) ~init: 0 in
    let to_strings = string_mat_to_strings string_matrix (max_len + 1) in
    Out_channel.write_lines filename to_strings

  let test_row () =
    let my_array = [| "1"; "2"; "3"; "4" |] in
    let result = row_concat my_array 2 in
    assert (result = " 1 2 3 4")
  
  let test_mat () =
    let my_mat = [| [| "1"; "2"; "3"; "4" |] ; [| "2"; "3"; "4"; "5"; |] |] in
    let result = string_mat_to_strings my_mat 2 in
    assert (result = [" 1 2 3 4"; " 2 3 4 5"])

  let test_mat_to_file () =
    let my_mat = M.big_test_matrix in
    mat_to_file my_mat "joe.txt"

  let run_tests () =
    test_row ();
    test_mat ();
    test_mat_to_file ();
    ()
end

module IntMatrixModule = IntWrite(IntMatrix);;

IntMatrixModule.run_tests();;


module StringMatrixModule = IntWrite(StringMatrix);;

StringMatrixModule.run_tests();;

(* Signature for reading a matrix. Note that, like IntWrite, the main issue 
 * with this module is its lack of abstraction. This will come shortly. Also,
 * style will also be fixed in the future. *)

(* On abstraction: Note that, because this module utilizes multiple data
 * structures, i.e. two dicts and a matrix, that abstractions could be made 
 * to both the type of dict used and the type of ranking used *)

module IntRead(D: DICT) : (READ with type key = D.key
  with type value = D.value with type dict = D.dict) = 
struct

  type key = D.key
  type value = D.value
  type dict = D.dict

  type mat_value = int
  type vec = int array
  type mat = vec array

  (* instantiates a square matrix of len dimensions *)
  let rank_matrix (len : int) = Array.create ~len:len (Array.create ~len:len 0)
  
  (* a ref for storing owners and their indices *)
  let owner_dict = ref (D.empty)
  (* a ref for storing the current max owner index *)
  let owner_index = ref 0

  (* a ref for storing elements and their indices *)
  let elt_dict = ref (D.empty)
  (* a ref for storing the current max element index *)
  let elt_index = ref 0

  (* converts the element-rank line into a (string, int) tuple by splitting at
   * the colon *)
  let process_elt (line : string) =
    let (a, b) = String.rsplit2_exn line ':' in
    (String.strip a, int_of_string (String.strip b))
    
  let process_file (filename : string) =
    let file_lines = In_channel.read_lines filename in
    (* checks if the line is for an owner or element by looking for a colon in
     * the line; this is an invariant of the .txt file *)
    let elt_check (line : string) = String.contains line ':' in
    let owner_num = List.fold_right file_lines 
      ~f:(fun x acc -> if not (elt_check x) then acc + 1 else acc) ~init:0 in
    let return_matrix = rank_matrix owner_num in
    let add_to_dict (subject : string) : unit =
      if not (elt_check subject) then (
	if D.member (!owner_dict) (D.key_of_string (String.strip subject)) then
	  failwith "owner already in dict"
	else (owner_dict := D.insert (!owner_dict) (D.key_of_string (String.strip subject)) (D.val_of_int (!owner_index));
	      owner_index := (!owner_index) + 1))
      else (
	let (a, b) = process_elt subject in
	match D.lookup (!elt_dict) (D.key_of_string a) with
	| None -> (
	   elt_dict := D.insert (!elt_dict) (D.key_of_string a) (D.val_of_int (!elt_index));
	   (return_matrix.(!owner_index - 1)).(!elt_index) <- b;
	   elt_index := (!elt_index) + 1)
	| Some x ->
	   (return_matrix.(!owner_index - 1)).(int_of_string (D.string_of_value x))
	   <- b) in
    List.iter file_lines ~f:add_to_dict;
    return_matrix
(*
  let test_read () = mat_to_file (process_file "jane.txt") "test_mat.txt"

  let run_tests () =
    test_read ();
    ()
 *)
end
