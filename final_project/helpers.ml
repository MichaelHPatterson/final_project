(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * helpers.ml -- misc. helper functions *)

open Core.Std

module type MATRIX_ARG =
sig
  (* type for matrix implementation *)
  type value

  val string_of_val : value -> string

  val val_of_string : string -> value

  val zero_value : value

  val big_test_matrix : value array array
end

module type PRINT =
sig
  type value

  val print_matrix : value array array -> unit
end

(* A basic matrix command-line printing function for debugging that prints a
 * matrix of arbitrary type. Rows will be displayed one elt per line, and they
 * are separated by a newline. Feel free to modify this so it's more readable
 * in the terminal. Also note that the module is needed because of the need for 
 * a matrix functor *)

module Printer (M: MATRIX_ARG) : (PRINT with type value = M.value) =
struct
  type value = M.value

  let print_matrix (matrix : 'a array array) : unit =
    let print_line (my_row : 'a array) : unit =
      let print_cell rank =
	let my_string = (M.string_of_val rank) ^ "\n" in
	Out_channel.output_string stdout my_string in
      Array.iter my_row ~f:print_cell;
      Out_channel.output_string stdout "\n";
      Out_channel.flush stdout in
    Array.iter matrix ~f:print_line;
    Out_channel.flush stdout
end

let get_mat (tuple : ('a * 'b * 'c)) : 'a = 
  match tuple with
  | (x, _, _) -> x

let get_own_dict (tuple : ('a * 'b * 'c)) : 'a = 
  match tuple with
  | (_, x, _) -> x

let get_elt_dict (tuple : ('a * 'b * 'c)) : 'a = 
  match tuple with
  | (_, _, x) -> x

(* general fold function for matrices *)
let matrix_fold (matrix : 'b array array) ~(f : 'a -> 'b -> 'a) ~(init : 'a)
    : 'a =
  Array.fold matrix ~init:init ~f:(fun acc x -> Array.fold x ~init:acc ~f:f)

(* general map function for matrices *)
let matrix_map (matrix : 'a array array) ~(f : 'a -> 'b) : 'b array array =
  let len = Array.length matrix in
  let result = Array.make_matrix ~dimx:len ~dimy:len (f (matrix.(0).(0))) in
  for i = 0 to len -1 do
    result.(i) <- Array.map ~f:f matrix.(i)
  done;
  result
