(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .txt files *)

open Core.Std

(* Signature for writing to file. Note that the only exposed function is actually
 * writing to a file. This provides a favorable abstraction layer. *)

module type WRITE =
sig
  (* types for the matrix implementation *)
  type value
  type vec
  type mat

  (* writes a whole matrix to a .txt file of given name *)
  val mat_to_file : mat -> string -> unit

  (* runs tests for writing *)
  val run_tests : unit -> unit

end



(* Signature for reading from a file. Note that the only exposed function is
 * actually reading the file, favorable for abstraction. *)

module type READ =
sig
  (* types for the matrix implementation *)
  type value
  type vec
  type mat

  (* reads a .txt file of a specific invariant, returning the ranking matrix *)
  val process_file : string -> mat

  (* runs tests for writing *)
  val run_tests : unit -> unit

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

module IntWrite : WRITE  =
struct
  type value = int
  type vec = value array
  type mat = vec array

  let matrix_fold (matrix : 'b array array) ~(f : 'a -> 'b -> 'a) ~(init : 'a) : 'a =
    Array.fold matrix ~init:init ~f:(fun acc x -> Array.fold x ~init:acc ~f:f)

  let matrix_map matrix f =
    let len = Array.length matrix in
    let result = Array.create ~len:len (Array.create ~len:len (f (matrix.(0)).(0))) in
    for i = 0 to len - 1 do
      result.(i) <- Array.map f matrix.(i)
    done;
    result

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

  let string_mat_to_strings (matrix : string array array) (space : int) : string list  =
    Array.fold_right matrix ~f:(fun x acc -> (row_concat x space) :: acc) ~init:[]

  let mat_to_file (matrix : mat) (filename : string) : unit =
    let string_matrix = matrix_map matrix string_of_int in
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
    let my_mat = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] ; [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |] in
    mat_to_file my_mat "bob.txt"

  let run_tests () =
    test_row ();
    test_mat ();
    test_mat_to_file ();
    ()
    
end



(* Signature for reading a matrix. Note that, like IntWrite, the main issue 
 * with this module is its lack of abstraction. This will come shortly. Also,
 * style will also be fixed in the future. *)

(* On abstraction: Note that, because this module utilizes multiple data
 * structures, i.e. two dicts and a matrix, that abstractions could be made 
 * to both the type of dict used and the type of ranking used *)

module IntRead : READ = 
struct
  open IntWrite
  type value = int
  type vec = int array
  type mat = vec array

  module SDict = Psetdict.Make (Psetdict.StringIntDictArg)
	       
  let rank_matrix (len : int) = Array.create ~len:len (Array.create ~len:len 0)

  let owner_dict = ref (SDict.empty)
  let owner_index = ref 0

  let elt_dict = ref (SDict.empty)
  let elt_index = ref 0

  let process_elt (line : string) : (string, int) =
    let string_list = List.map (String.rsplit2_exn line ':') String.strip in
    match string_list with
    | a :: b :: [] -> (a, int_of_string b)
    | _ -> failwith "not correct formatting"
    
  let process_file (filename : string) =
    let file_lines = In_channel.read_lines filename in
    (* checks if the line is for an owner or element by looking for a colon in
     * the line; this is an invariant of the .txt file *)
    let elt_check (line : string) = String.contains line ':' in
    let owner_num = List.fold_right file_lines 
      ~f:(fun x acc -> if elt_check x then acc else 1 + acc) ~init:0 in
    let return_matrix = rank_matrix owner_num in
    let add_to_dict (subject : string) : unit =
      if not (elt_check subject) then (
	if SDict.member (!owner_dict) subject then
	  failwith "owner already in dict"
	else (owner_dict := SDict.insert (!owner_dict) (!owner_index);
	      owner_index := (!owner_index) + 1))
      else (
	let (a, b) = process_elt subject in
	match SDict.lookup (!elt_dict) a with
	| None -> (
	   elt_dict := SDict.insert (!elt_dict) a elt_index;
	   (return_matrix.(!owner_index)).(!elt_index) <- b;
	   elt_index := (!elt_index) + 1)
	| Some x ->
	   (return_matrix.(!owner_index)).(x) <- b) in
    List.iter file_lines ~f:add_to_dict;
    return_matrix

  let test_read () = mat_to_file (process_file "jane.txt") "test_mat.txt"

  let run_tests () =
    test_read ();
    ()

end
