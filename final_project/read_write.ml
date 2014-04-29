(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .txt files *)

open Core.Std
open Psetdict
open Matrixops

exception TODO
exception SizeMismatch
exception IndexOutOfBounds
exception NotSquare
exception InversionError

(* a "helper functor" that is used for most tasks involving matrices *)
module type MATRIX_ARG =
sig
  (* type for matrix implementation *)
  type value = float

  val string_of_val : value -> string

  val val_of_string : string -> value

  val big_test_matrix : value array array
end


(* Signature for writing to file. The only exposed functions are writing to a
 * file and running tests, which provides a favorable abstraction layer. *)
module type WRITE =
sig
  (* types for the matrix implementation *)
  type value = float
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
  type mat_value = float
  type vec = mat_value array
  type mat = vec array

  (* reads a .txt file of a specific invariant, returning the ranking matrix *)
  val process_file : string -> (mat * dict * dict)
end
 
module FloatMatrix : MATRIX_ARG =
struct
  type value = float
  type vec = value array
  type mat = vec array

  let string_of_val = Float.to_string

  let val_of_string = Float.of_string

  let big_test_matrix = [| [| 1.5; 2.0; 3.0; 4.0 |]; [| 5.0; 6.0; 7.0; 8.0 |];
    [| 9.0; 10.0; 11.0; 12.0 |]; [| 13.0; 14.0; 15.0; 16.0 |] |]
end


(* Writes matrices of some type given by M to a text file. *)
module Write(M: MATRIX_ARG) : WRITE =
struct
  type value = float
  type vec = value array
  type mat = vec array

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
  let string_mat_to_strings (matrix : string array array) (space : int)
      : string list  =
    Array.fold_right matrix ~f:(fun x acc -> (row_concat x space) :: acc)
      ~init:[]

  (* converts matrix to string matrix, formats, and writes to file *)
  let mat_to_file (matrix : mat) (filename : string) : unit =
    let string_matrix = Helpers.matrix_map matrix ~f:M.string_of_val in
    let max_len = Helpers.matrix_fold string_matrix ~f:(fun acc x -> max 
      (String.length x) acc) ~init: 0 in
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
    mat_to_file my_mat "int_write_test.txt"

  let run_tests () =
    test_row ();
    test_mat ();
    test_mat_to_file ();
    ()
end

module FloatMatrixModule = Write(FloatMatrix);;
FloatMatrixModule.mat_to_file(FloatMatrix.big_test_matrix) 
  "float_write_test.txt";;


(* Signature for reading a file and building a matrix, with dicts for indexing
 * the rows and columns. Requires a matrix functor, which defines the type of 
 * the matrix. Also requires a dict functor, which defines the dict that is 
 * stored. *)
module Read (M: MATRIX_ARG) (D: DICT) : (READ with type key = D.key
  with type value = D.value with type dict = D.dict) = 
struct

  type key = D.key
  type value = D.value
  type dict = D.dict

  type mat_value = float
  type vec = mat_value array
  type mat = vec array

  (* instantiates a square matrix of len dimensions *)
  let rank_matrix (len : int) = Array.make_matrix len len 0.0
  
  (* The following code  fucks things up -- all of the columns become uniform.
   * I think this is because the m.(x).(y) notation doesn't work for normal
   * two-dimensional arrays. However, it works for the make_matrix two-
   * dimensional arrays.
   * Array.create ~len:len (Array.create ~len:len 0.0) *)
  
  (* converts the element-rank line into a (string, mat_value) tuple by 
   * splitting at the colon *)
  let process_elt (line : string) =
    let (a, b) = String.rsplit2_exn line ':' in
    (String.strip a, M.val_of_string (String.strip b))
    
  let process_file (filename : string) : (mat * dict * dict) =
    (* a ref for storing owners and their indices *)
    let owner_dict : dict ref = ref (D.empty) in
    (* a ref for storing the current max owner index *)
    let owner_index : int ref = ref 0 in

    (* a ref for storing elements and their indices *)
    let elt_dict : dict ref = ref (D.empty) in
    (* a ref for storing the current max element index *)
    let elt_index : int ref = ref 0 in
   
    (* a string list of all lines in the file*)
    let file_lines : string list = In_channel.read_lines filename in

    (* checks if the line is for an owner or element by looking for a colon in
     * the line; this is an invariant of the .txt file *)
    let elt_check (line : string) : bool = String.contains line ':' in

    (* checks how many owners are in the .txt file and returns a square matrix
     * of the same dimension as this quantity *)
    let owner_num : int = List.fold_right file_lines 
      ~f:(fun x acc -> if not (elt_check x) then acc + 1 else acc) ~init:0 in
    let return_matrix = rank_matrix owner_num in
    
    (* updates a specific point in the matrix *)
    let update_matrix (matrix : mat) (row : int) (col : int) 
      (new_val : mat_value) : unit = matrix.(row).(col) <- new_val in

    (* adds owners and elts to their respective dicts, and adds rankings of elts
     * by owners to their specific index in a matrix *)
    let add_to_dict (subject : string) : unit =
      let subj = String.strip subject in
      (* if a line is empty, it's not an owner or elt *)     
      if (subj = "") then () else
      (* if it's an owner then add that owner to dict, or if owner is already in
       * dict then return error *)
      if not (elt_check subject) then (
	if D.member (!owner_dict) (D.key_of_string subj) then
	  failwith "owner already in dict"
	else (owner_dict := D.insert (!owner_dict) (D.key_of_string subj) 
               (D.val_of_int (!owner_index)));
	      owner_index := (!owner_index) + 1)
      (* if it's an elt than either add to the dict and matrix if it's a new elt
       * or just add to the matrix if it already has an index *)
      else (
	let (a, b) = process_elt subject in
	match D.lookup (!elt_dict) (D.key_of_string a) with
	| None -> (
	   elt_dict := D.insert (!elt_dict) (D.key_of_string a) 
             (D.val_of_int (!elt_index));
	   update_matrix return_matrix (!owner_index - 1) (!elt_index) b;
	   elt_index := (!elt_index) + 1)
	| Some x ->
	   update_matrix return_matrix (!owner_index - 1) 
	     (int_of_string (D.string_of_value x)) b) in

    List.iter file_lines ~f:add_to_dict;
    (return_matrix, (!owner_dict), (!elt_dict))
end

(* Tests with Matrix operations *)
module FloatRead = Read (FloatMatrix) (Make(StringIntDictArg));;
module FloatWrite = Write (FloatMatrix);;

FloatWrite.mat_to_file (Helpers.get_mat (FloatRead.process_file 
  "test_float_input.txt")) "test_output3.txt";;

let my_float_matrix = Helpers.get_mat (FloatRead.process_file
 "test_float_input.txt");;

FloatWrite.mat_to_file ((FloatOps.add_mat my_float_matrix 
  (FloatOps.identity 5))) "test_output4.txt";;
