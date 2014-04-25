(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .csv files *)

open Core.Std
(* open Out_channel *)

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
(*
module type WRITE =
sig
  (* types for the matrix implementation *)
  type value
  type vec
  type mat
  (*
  (* writes a row of a matrix (or vector) to a .txt file *)
  val row_to_string : vec -> string list
   *)
  (* writes a whole matrix to a .txt file of given name *)
  val mat_to_file : mat -> string -> string list list

  (* runs tests for writing *)
  val run_tests : unit -> unit

end
 *)
module IntWrite =
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

(*
    let copy = Array.copy 

    let len = Array.length matrix in
    let result = Array.create ~len:len (Array.create ~len:len f (matrix.(i)).(j)) in
    Array.iter matrix (
		 for i = 0 to len - 1 do
		   (result.(
    for i = 0 to len - 1 do
	for j = 0 to len - 1 do
	  (result.(i)).(j) <- f (matrix.(i)).(j)
	done;
    done;
    result
 *)

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


    let to_string_fun = fun acc x -> (string_of_int x) :: acc in
    let string_list = List.rev (Array.fold row ~init:[] ~f:to_string_fun) in
    (* builds the string to be put in a file *)
    let rec build_string lst : string =
      (* space maker *)
      let rec make_space (empties : int) : string =
	if empties < 0 then "" else
	match empties with
	| 0 -> ""
	| _ -> " " ^ make_space (empties - 1) in
      match lst with
      | [] -> ""
      | x :: xs -> (make_space (space - (String.length x))) ^ x ^ build_string xs in
    build_string string_list

  let  (matrix : mat) : string list =
    let max_length = 
    List.rev (Array.fold matrix ~init:[] ~f:(fun acc x -> (row_to_string x 4) :: acc))

  let test_row () =
    let my_array = [| 1; 2; 3; 4 |] in
    let result = row_to_string my_array in
    assert (result = "1 2 3 4")
  
  let test_mat () =
    let my_mat = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |] in
    let result = mat_to_string my_mat in
    assert (result = ["1 2 3 4"; "2 3 4 5"])
	
  let test_mat_to_file () =
    let my_mat = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |] in
    mat_to_file my_mat "bob.txt"
 
  let run_tests () =
    test_row ();
    test_mat ();
    test_mat_to_file ();
    ()


end
 


  let mat_to_file (matrix : mat) (filename : string) : unit =
    let string_matrix = matrix_map (string_of_int) in
    let max_len = matrix_fold string_matrix ~f:(fun x acc -> max (String.length x) acc) ~init: 0 in
    
    

  let new_mat_to_string (matrix : mat) =
    matrix_map matrix (string_of_int)

  let row_to_string (vector : vec) =
    let to_string_fun = fun acc x -> (string_of_int x) :: acc in
    let string_list = List.rev (Array.fold vector ~init:[] ~f:to_string_fun) in
    String.concat ~sep:" " string_list

  let mat_to_string (matrix : mat) =
    List.rev (Array.fold matrix ~init:[] ~f:(fun acc x -> (row_to_string x) :: acc))

  let mat_to_file (matrix : mat) (filename : string) : unit =
    let string_list = mat_to_string matrix in
    Out_channel.write_lines filename string_list

  let test_row () =
    let my_array = [| 1; 2; 3; 4 |] in
    let result = row_to_string my_array in
    assert (result = "1 2 3 4")
  
  let test_mat () =
    let my_mat = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |] in
    let result = mat_to_string my_mat in
    assert (result = ["1 2 3 4"; "2 3 4 5"])
	
  let test_mat_to_file () =
    let my_mat = [| [| 1; 2; 3; 4 |] ; [| 2; 3; 4; 5; |] |] in
    mat_to_file my_mat "bob.txt"
 
  let run_tests () =
    test_row ();
    test_mat ();
    test_mat_to_file ();
    ()
end
