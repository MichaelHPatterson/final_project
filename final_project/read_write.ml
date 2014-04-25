(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .csv files *)

open Core.Std

(*
module type READ =
sig
  (* types for the matrix implementation *)
  type value
  type vec
  type mat

  (* reads a line from the .csv file and loads it into memory *)
  val read_line : in_channel -> mat -> unit

  (* reads a whole .csv file and loads it into memory *)
  val read_file : in_channel -> mat -> unit
end
 *)


(* Signature for writing to file. Note that the only exposed function is actually'
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
