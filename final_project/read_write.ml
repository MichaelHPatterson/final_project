(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * read_write.ml -- provides read/write functionality from/to .csv files *)

open Core.std

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

module type WRITE =
sig
  (* types for the matrix implementation *)
  type value
  type vec
  type mat

  (* writes a row of a matrix to a .csv file *)
  val write_row : mat -> out_channel -> unit

  (* writes a whole matrix to a .csv file *)
  val write_mat : mat -> out_channel -> unit
end
