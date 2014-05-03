(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * main.ml -- file that runs the contents of an input file of rankings 
 * through the PageRank and Hungarian algorithms *)

open Core.Std
open Read_write


let functor_ind = ref 0;;

let ranking1 = (module FloatMatrixArg : MATRIX_ARG);;
let ranking2 = (module FloatMatrix5Arg : MATRIX_ARG);;
let ranking3 = (module FloatLikeArg : MATRIX_ARG);;

let my_functor_list = [ranking1; ranking2; ranking3];;

let rec get_functor x lst =
  match lst with
  | y :: ys -> if x <= 0 then y else get_functor (x - 1) ys
  | _ -> failwith "ran out of elts in list";;

let my_functor = get_functor (!(functor_ind)) my_functor_list;;

module MatrixRank = (val my_functor : MATRIX_ARG);;

module FloatRead = Read(MatrixRank)(MakeDict);;
module FloatWrite = Write(MatrixRank)(MakeDict);;

(*
module FloatRead = Read(FloatMatrixArg)(MakeDict);;
module FloatWrite = Write(FloatMatrixArg)(MakeDict);;
 *)
let run_algorithms (input : string) (output : string) : unit =
  try (
    let time1 = Unix.gettimeofday () in
    let (input_mat, owner_dict, elt_dict) = FloatRead.process_file input in
    let formatted_input = FloatWrite.mat_formatted input_mat in
    let time2 = Unix.gettimeofday () in
    let pagerank_mat = Pagerank.pageranks(input_mat) in
    let time3 = Unix.gettimeofday () in
    let pagerank_formatted = FloatWrite.mat_formatted pagerank_mat in
    let time4 = Unix.gettimeofday () in
    let cost_convert = Pagerank.cost_matrix(pagerank_mat) in
    let time5 = Unix.gettimeofday () in
    let cost_formatted = FloatWrite.mat_formatted cost_convert in
    let time6 = Unix.gettimeofday () in
    let hungarian_results = Hungarian.hungarian(cost_convert) in
    let time7 = Unix.gettimeofday () in
    let hungarian_formatted = Hungarian.format_hungarian hungarian_results
							 owner_dict elt_dict in
    let time8 = Unix.gettimeofday () in
    let formatted_output = List.concat [["Input Matrix of Rankings:"]; 
      formatted_input; ["Matrix of Pageranks:"]; pagerank_formatted;
      ["Cost Matrix"]; cost_formatted; ["Hungarian Algorithm Results"];
      hungarian_formatted] in
    let time9 = Unix.gettimeofday () in
    FloatWrite.data_to_file (input_mat, owner_dict, elt_dict) "saved_output";
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
    let keyified_owner = MakeDict.key_of_string owner in
    let keyified_elt = MakeDict.key_of_string elt in
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
  let match_rank_type = function
    | "0to10" -> functor_ind := 0;
    | "0to5" -> functor_ind := 1;
    | "LiketoDislike" -> functor_ind := 2;
    | _ -> Printf.printf "Invalid ranking type! You must choose 0to10, ";
	   Printf.printf "0to5, or LiketoDislike.\n";
	   exit 1 in
  let usage () =
    let main = "usage " ^ Sys.argv.(0) in
    let my_string = main ^ " [input file] [output file] [rank method] OR \n" ^
      main ^ " update [input file] [owner] [elt] [value] [rank method] OR \n" ^
	main ^ " remove [input file] [owner] [elt] [rank method] \n" in
    Out_channel.output_string stdout my_string in
  match Array.length Sys.argv with
  | 4 -> match_rank_type (Sys.argv.(3));
	 run_algorithms Sys.argv.(1) Sys.argv.(2);
  | 6 -> 
     (match Sys.argv.(1) with
      | "remove" -> match_rank_type (Sys.argv.(5));
		    remove_rating Sys.argv.(2) Sys.argv.(3) Sys.argv.(4);
      | _ -> Printf.printf "Invalid argument: expected \"remove\" as the ";
	     Printf.printf "first argument.\n")
  | 7 -> 
     (match Sys.argv.(1) with
      | "update" -> 
	 match_rank_type (Sys.argv.(6));
	 update_rating Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) 
           (Float.of_string Sys.argv.(5));
      | _ -> Printf.printf "Invalid argument: expected \"update\" as the ";
	     Printf.printf "first argument.\n")
  | _ -> usage ();;

parse_args ();;
