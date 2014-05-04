(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .txt files *)

open Core.Std
open Psetdict
open Matrix.FloatMatrix

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
		
  (* the default value for when an elt does not have a ranking for an owner *)
  val default : value

  (* the maximum and minimum values of our ranking system. If a value is found
   * that is found outside these bounds, then throw an error *)
  val min : value
  val max : value
end


(* Signature or writing to file. The only exposed functions are writing to a
 * file and running tests, which provides a favorable abstraction layer. *)
module type WRITE =
sig
  (* types for dictionary use*)
  type key
  type value
  type dict

  (* types for the matrix implementation *)
  type mat_value = float
  type vec = mat_value array
  type mat = vec array

  (* prepares a whole matrix for writing to a file by formatting the matrix to
   * be written into a list of strings to be printed *)
  val mat_formatted : mat  -> string list

  (* writes the matrix, owner dict, and elt dict to the invariant that the data
   * is originally read from *)
  val data_formatted : (mat * dict * dict) -> string list

  (* writes matrix, owner dict, and elt dict directly to a file *)
  val data_to_file : (mat * dict * dict) -> string -> unit

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

  val default : mat_value

  (* reads a .txt file of a specific invariant, returning the ranking matrix
   * Note: The ranking matrix will have the owners as rows and the elements as
   * columns. *)
  val process_file : string -> (mat * dict * dict)
end


(* A functor for 0-10 rankings. *)
module FloatMatrixArg : MATRIX_ARG =
struct
  type value = float

  let string_of_val = Float.to_string

  let val_of_string = Float.of_string

  let default = 5.
  let min = 0.
  let max = 10.
end

(* Note that, in its default configuration, the following two ranking types are
 * not used. However, a simple change in main.ml will allow them to be used,
 * albeit only one at a time. *)
(* A functor for 0-5 rankings. *)
module FloatMatrix5Arg : MATRIX_ARG =
struct
  type value = float

  let string_of_val = Float.to_string

  let val_of_string = Float.of_string

  let default = 2.5
  let min = 0.
  let max = 5.
end

(* See the comment above FloatMatrix5Arg *)
(* A functor for Like/Dislike ratings *)
module FloatLikeArg : MATRIX_ARG =
struct
  type value = float

  let string_of_val my_float =
    match my_float with
    | 0. -> "None"
    | 1. -> "Like"
    | -1. -> "Dislike"
    | _ -> failwith "invalid value for Like ratings"

  let val_of_string my_string =
    match my_string with
    | "None" -> 0.
    | "Like" -> 1.
    | "Dislike" -> -1.
    | _ -> failwith "invalid value for Like string"

  let default = 0.

  let min = -1.0
  let max = 1.0
end


(* Writes matrices of some type given by M to a text file. *)
module Write(M: MATRIX_ARG) (D: DICT) : (WRITE with type key = D.key
  with type value = D.value with type dict = D.dict) =
struct
  type key = D.key
  type value = D.value
  type dict = D.dict

  type mat_value = float
  type vec = mat_value array
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
   
  (* formats a matrix for display, but does not write to file *)
  let mat_formatted (matrix : mat) : string list =
    let string_matrix = Helpers.matrix_map matrix ~f:M.string_of_val in
    let max_len = Helpers.matrix_fold string_matrix ~f:(fun acc x -> max 
      (String.length x) acc) ~init: 0 in
    let to_strings = string_mat_to_strings string_matrix (max_len + 1) in
      to_strings

  (* converts the matrix, owner dict, and elt dict into the file invariant of
   * inputs *)
  let data_formatted (input : (mat * dict * dict)) : string list =
    let (input_mat, owner_dict, elt_dict) = input in
    let key_value_list (d: dict) = D.fold (fun k v acc -> ((D.string_of_key k),
      (D.int_of_val v)) :: acc) [] d in
    let snd_sort a b : int =
      let (_, x) = a in
      let (_, y) = b in
      Int.compare x y in
    let sort_owners_by_vals = 
      List.to_array (List.sort ~cmp:snd_sort (key_value_list owner_dict)) in
    let sort_elts_by_vals = 
      List.to_array (List.sort ~cmp:snd_sort (key_value_list elt_dict)) in

    let format_owners (input : (string * int)) : string list =
      let (owner_string, owner_ind) = input in
      let owner_row = input_mat.(owner_ind) in
      let string_format (elt_str : string) (rank_str : string) : string =
	elt_str ^ " : " ^ rank_str in
      let get_string a : string =
	let (x, _) = a in x in
      let current_elt (x : int) : string = 
	get_string (sort_elts_by_vals.(x)) in
      let list_of_rankings = Array.fold owner_row ~init:([],0) ~f:(
       fun acc x -> let (lst, index) = acc in
         ((string_format (current_elt index) (M.string_of_val x)) :: lst,
	  index + 1)) in
      owner_string :: (List.rev (fst list_of_rankings)) in

    let my_strings = Array.fold sort_owners_by_vals ~init:[] ~f:(
      fun acc x -> (format_owners x) :: acc) in
    let lists_append (lst : string list list) : string list =
      List.fold_right lst ~f:(fun x acc -> x @ acc) ~init:[] in
    let formatted_out = lists_append (List.rev my_strings) in
    formatted_out

  let data_to_file (input : (mat * dict * dict)) (file : string) : unit =
    let string_list = data_formatted input in
    Out_channel.write_lines file string_list

  let test_row () =
    let my_array = [| "1"; "2"; "3"; "4" |] in
    let result = row_concat my_array 2 in
    assert (result = " 1 2 3 4")
  
  let test_mat () =
    let my_mat = [| [| "1"; "2"; "3"; "4" |] ; [| "2"; "3"; "4"; "5"; |] |] in
    let result = string_mat_to_strings my_mat 2 in
    assert (result = [" 1 2 3 4"; " 2 3 4 5"])

  let run_tests () =
    test_row ();
    test_mat ();
    ()
end


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

  let default = M.default

  (* instantiates a square matrix of len dimensions *)
  let rank_matrix (len : int) = Array.make_matrix ~dimx:len ~dimy:len M.default
  
  (* The following code screws things up -- all of the columns become uniform.
   * I think this is because the m.(x).(y) notation doesn't work for normal
   * two-dimensional arrays. However, it works for the make_matrix two-
   * dimensional arrays.
   * Array.create ~len:len (Array.create ~len:len 0.0) *)

  (* converts the element-rank line into a (string, mat_value) tuple by 
   * splitting at the colon. Also, checks that all rankings are in the bounds of
   * the limits in the matrix functor *)
  let process_elt (line : string) =
    let (a, b) = String.rsplit2_exn line ~on:':' in
    let check_bounds my_val = my_val >= M.min && my_val <= M.max in
    let processed = M.val_of_string (String.strip b) in
    if check_bounds processed then (String.strip a, processed)
    else failwith "ranking not in bounds"
    
  let process_file (filename : string) : (mat * dict * dict) =
    (* a ref for storing owners and their indices *)
    let owner_dict : dict ref = ref D.empty in
    (* a ref for storing the current max owner index *)
    let owner_index : int ref = ref 0 in

    (* a ref for storing elements and their indices *)
    let elt_dict : dict ref = ref D.empty in
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
      if subj = "" then ()
      (* if it's an owner then add that owner to dict, or if owner is already in
       * dict then return error *)
      else if not (elt_check subject) then
	if D.member !owner_dict (D.key_of_string subj) then
	  failwith "owner already in dict"
	else (owner_dict := D.insert !owner_dict (D.key_of_string subj) 
               (D.val_of_int !owner_index);
	      owner_index := !owner_index + 1)
      (* if it's an elt than either add to the dict and matrix if it's a new elt
       * or just add to the matrix if it already has an index *)
      else
	let (a, b) = process_elt subject in
	match D.lookup (!elt_dict) (D.key_of_string a) with
	| None -> (
	   elt_dict := D.insert !elt_dict (D.key_of_string a) 
             (D.val_of_int !elt_index);
	   update_matrix return_matrix (!owner_index - 1) !elt_index b;
	   elt_index := !elt_index + 1)
	| Some x ->
	   update_matrix return_matrix (!owner_index - 1) 
	     (D.int_of_val x) b in
    List.iter file_lines ~f:add_to_dict;
    (return_matrix, !owner_dict, !elt_dict)
end

module MakeDict = Make(StringIntDictArg);;
