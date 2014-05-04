(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * main.ml -- file that runs the contents of an input file of rankings 
 * through the PageRank and Hungarian algorithms *)

open Core.Std
open Read_write



(* Note: The functors of the following modules are the only variables that must
 * be changed to allow for different ranking types and dictionaries. *)
module MatrixRank = FloatMatrixArg;;
module FloatRead = Read(MatrixRank)(MakeDict);;
module FloatWrite = Write(MatrixRank)(MakeDict);;

let run_algorithms (input : string) (output : string) : unit =
  try (
    let (input_mat, owner_dict, elt_dict) = FloatRead.process_file input in
    let formatted_input = 
      FloatWrite.data_formatted (input_mat, owner_dict, elt_dict) in
    let pagerank_mat = Pagerank.pageranks(input_mat) in
    let pagerank_formatted = 
      FloatWrite.data_formatted (pagerank_mat, owner_dict, elt_dict) in
    let cost_convert = Pagerank.cost_matrix(pagerank_mat) in
    let cost_formatted = 
      FloatWrite.data_formatted (cost_convert, owner_dict, elt_dict) in
    let hungarian_results = Hungarian.hungarian(cost_convert) in
    let hungarian_formatted = Hungarian.format_hungarian hungarian_results
							 owner_dict elt_dict in
    let formatted_output = List.concat [["Input Rankings:\n"]; 
      formatted_input; ["\nPagerank Rankings:\n"]; pagerank_formatted;
      ["\nCost Matrix\n"]; cost_formatted; ["\nHungarian Algorithm Results\n"];
      hungarian_formatted] in
    Out_channel.write_lines output formatted_output)
  with (Sys_error error) ->
    if error = input ^ ": No such file or directory" then
      Printf.printf "The input file does not exist!\n"
    else
      Printf.printf "An error occurred: \"%s\"\n" error

let update_rating (file : string) (owner: string) (elt : string) 
  (updated_value : FloatRead.mat_value) : unit =
  try (try (
    let (input_mat, owner_dict, elt_dict) = FloatRead.process_file file in
    let check_bounds my_val = my_val >= MatrixRank.min &&
			      my_val <= MatrixRank.max in
    if check_bounds updated_value then ()
    else (Printf.printf "Your ranking was out of bounds!\n"; exit 1);
    let keyified_owner = MakeDict.key_of_string (String.strip owner) in
    let keyified_elt = MakeDict.key_of_string(String.strip elt) in
    let owner_index = MakeDict.lookup owner_dict keyified_owner in
    let elt_index = MakeDict.lookup elt_dict keyified_elt in
    let update_matrix (matrix : FloatRead.mat) (row : int) (col : int) 
		      (new_val : FloatRead.mat_value) : unit =
       matrix.(row).(col) <- new_val in
    match (owner_index, elt_index) with
    | (None, None) ->
       Printf.printf "Invalid owner and elt string -- not in file!\n"
    | (None, Some _) -> Printf.printf "Invalid owner string -- not in file!\n"
    | (Some _, None) -> Printf.printf "Invalid elt string -- not in file!\n"
    | (Some x, Some y) ->
       update_matrix input_mat (MakeDict.int_of_val x)
		     (MakeDict.int_of_val y) updated_value;
       FloatWrite.data_to_file (input_mat, owner_dict, elt_dict) file)
  with (Sys_error error) ->
    if error = file ^ ": No such file or directory" then
      Printf.printf "That file does not exist!\n"
    else Printf.printf "An error occurred: \"%s\"\n" error)
  with (Failure error) ->
    if error = "ranking not in bounds" then
      Printf.printf "Your ranking was out of bounds!\n"
    else Printf.printf "An error occurred: \"%s\"\n" error
  
let remove_rating (file : string) (owner : string) (elt : string) : unit =
  update_rating file owner elt FloatRead.default

(* Parses command-line arguments, running the algorithms on the specified file.
 * Returns an error if incorrect number of args provided. *)
let parse_args () =
  let usage () =
    let main = "usage " ^ Sys.argv.(0) in
    let my_string = main ^ " [input file] [output file] OR \n" ^
      main ^ " update [input file] [owner] [elt] [value] OR \n" ^
	main ^ " remove [input file] [owner] [elt] \n" in
    Out_channel.output_string stdout my_string in
  match Array.length Sys.argv with
  | 3 -> run_algorithms Sys.argv.(1) Sys.argv.(2);
  | 5 -> 
     (match Sys.argv.(1) with
      | "remove" -> remove_rating Sys.argv.(2) Sys.argv.(3) Sys.argv.(4);
      | _ -> Printf.printf "Invalid argument: expected \"remove\" as the ";
	     Printf.printf "first argument.\n")
  | 6 -> 
     (match Sys.argv.(1) with
      | "update" -> 
	 update_rating Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) 
           (Float.of_string Sys.argv.(5));
      | _ -> Printf.printf "Invalid argument: expected \"update\" as the ";
	     Printf.printf "first argument.\n")
  | _ -> usage ();;

parse_args ();;
