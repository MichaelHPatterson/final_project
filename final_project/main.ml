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
  let time1 = Unix.gettimeofday () in
  let (input_mat, owner_dict, elt_dict) = FloatRead.process_file input in
  let formatted_input = 
    FloatWrite.data_formatted (input_mat, owner_dict, elt_dict) in
  let time2 = Unix.gettimeofday () in
  let pagerank_mat = Pagerank.pageranks(input_mat) in
  let time3 = Unix.gettimeofday () in
  let pagerank_formatted = 
    FloatWrite.data_formatted (pagerank_mat, owner_dict, elt_dict) in
  let time4 = Unix.gettimeofday () in
  let cost_convert = Pagerank.cost_matrix(pagerank_mat) in
  let time5 = Unix.gettimeofday () in
  let cost_formatted = 
    FloatWrite.data_formatted (cost_convert, owner_dict, elt_dict) in
  let time6 = Unix.gettimeofday () in
  let hungarian_results = Hungarian.hungarian(cost_convert) in
  let time7 = Unix.gettimeofday () in
  let hungarian_formatted = Hungarian.format_hungarian hungarian_results
    owner_dict elt_dict in
  let time8 = Unix.gettimeofday () in

  let lists_append (lst : string list list) : string list =
    List.fold_right lst ~f:(fun x acc -> x @ acc) ~init:[] in
  let formatted_output = lists_append [["Input Rankings:\n"]; 
    formatted_input; ["Pagerank Rankings:\n"]; pagerank_formatted;
    ["Cost Matrix\n"]; cost_formatted; ["Hungarian Algorithm Results\n"];
    hungarian_formatted] in
  let time9 = Unix.gettimeofday () in
  Out_channel.write_lines output formatted_output;
  let time10 = Unix.gettimeofday () in
  Printf.printf "Time for reading and formatting input: %f\n" (time2 -. time1);
  Printf.printf "Time for computing M*M^T and matrix exponential: %f\n" (time3 -. time2);
  Printf.printf "Time for formatting PageRanks for printing: %f\n" (time4 -. time3);
  Printf.printf "Time for converting to a cost matrix: %f\n" (time5 -. time4);
  Printf.printf "Time for formatting the cost matrix for printing: %f\n" (time6 -. time5);
  Printf.printf "Time for applying the Hungarian algorithm: %f\n" (time7 -. time6);
  Printf.printf "Time for formatting the Hungarian algorithm results for printing: %f\n" (time8 -. time7);
  Printf.printf "Time for combining all output for printing: %f\n" (time9 -. time8);
  Printf.printf "Time for printing to files: %f\n" (time10 -. time9);
  flush_all ())
  with (Sys_error error) ->
    if error = input ^ ": No such file or directory" then
      Printf.printf "The input file does not exist!\n"
    else
      Printf.printf "An error occurred.\n"

let update_rating (file : string) (owner: string) (elt : string) 
  (updated_value : FloatRead.mat_value) : unit =
  try (try (
    let (input_mat, owner_dict, elt_dict) = FloatRead.process_file file in
    let check_bounds my_val = my_val >= MatrixRank.min && my_val <=
        MatrixRank.max in
    if check_bounds updated_value then ()
    else failwith "updated value not in bounds";
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
    else Printf.printf "An error occurred.\n")
  with (Failure error) ->
    if error = "ranking not in bounds" then
      Printf.printf "Your ranking was out of bounds!\n"
    else Printf.printf "An error occurred.\n"
  
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
