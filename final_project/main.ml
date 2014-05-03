(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * main.ml -- file that runs the contents of an input file of rankings 
 * through the PageRank and Hungarian algorithms *)

open Core.Std
open Read_write

let run_algorithms (input : string) (output : string) : unit =
  let open Read_write in
  let (input_mat, owner_dict, elt_dict) = FloatRead.process_file input in
  let formatted_input = FloatWrite.mat_formatted input_mat in
  let pagerank_mat = Pagerank.pageranks(input_mat) in
  let pagerank_formatted = FloatWrite.mat_formatted pagerank_mat in
  let cost_convert = Pagerank.cost_matrix(pagerank_mat) in
  let cost_formatted = FloatWrite.mat_formatted cost_convert in
  let hungarian_results = Hungarian.hungarian(cost_convert) in
  let hungarian_formatted = Hungarian.format_hungarian hungarian_results owner_dict
    elt_dict in
  let lists_append (lst : string list list) : string list =
    List.fold_right lst ~f:(fun x acc -> x @ acc) ~init:[] in
  let formatted_output = lists_append [["Input Matrix of Rankings:"]; 
    formatted_input; ["Matrix of Pageranks:"]; pagerank_formatted;
    ["Cost Matrix"]; cost_formatted; ["Hungarian Algorithm Results"];
     hungarian_formatted] in
  FloatWrite.data_to_file (input_mat, owner_dict, elt_dict) "saved_output";
  Out_channel.write_lines output formatted_output

let update_rating (file : string) (owner: string) (elt : string) 
  (updated_value : FloatRead.mat_value) : unit =
  let (input_mat, owner_dict, elt_dict) = FloatRead.process_file file in
  let keyified_owner = MakeDict.key_of_string owner in
  let keyified_elt = MakeDict.key_of_string elt in
  let owner_index = MakeDict.lookup owner_dict keyified_owner in
  let elt_index = MakeDict.lookup elt_dict keyified_elt in
  let update_matrix (matrix : FloatRead.mat) (row : int) (col : int) 
    (new_val : FloatRead.mat_value) : unit = matrix.(row).(col) <- new_val in
  (match (owner_index, elt_index) with
  | (None, None) -> failwith "invalid owner and elt string -- not in file!"
  | (None, Some _) -> failwith "invalid owner string -- not in file!"
  | (Some _, None) -> failwith "invalid elt string -- not in file!"
  | (Some x, Some y) -> update_matrix input_mat (MakeDict.int_of_val x) 
      (MakeDict.int_of_val y) updated_value);
  FloatWrite.data_to_file (input_mat, owner_dict, elt_dict) file
  
let remove_rating (file : string) (owner : string) (elt : string) : unit =
  update_rating file owner elt FloatRead.default


(* Parses command-line arguments, running the algorithms on the specified file.
 * Returns an error if incorrect number of args provided. *)
let parse_args () =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 3 then usage ()
  else run_algorithms Sys.argv.(1) Sys.argv.(2);;

parse_args ();
