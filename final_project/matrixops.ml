(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * matrix.ml -- Provides interface for matrix manipulation (e.g  addition,
 * row reduction, etc.) *)

open Core.Std

exception TODO
exception SizeMismatch
exception IndexOutOfBounds
exception NotSquare
exception InversionError

(* a "helper functor" that is used for most tasks involving matrices *)
module type MATRIX_ARG =
sig
  (* type for matrix implementation *)
  type value
  type vec = value array
  type mat = vec array

  val string_of_val : value -> string

  val val_of_string : string -> value

  val float_of_val : value -> float

  val val_of_float : float -> value

  val zero_value : value

  val big_test_matrix : value array array
end


module type MATRIX =
sig
  type value
  type vec = value array
  type mat = vec array

  val string_of_val : value -> string

  val val_of_string : string -> value

  val float_of_val : value -> float

  val val_of_float : float -> value

  val zero_value : value

  val big_test_matrix : value array array

  val zero_vec : int -> vec

  val zero_mat : int -> int -> mat

  val basis_vec : dim:int -> int -> vec

  val identity : int -> mat

  val map2 : f:(value -> value -> 'a) -> vec -> vec -> 'a array

  val add_vec : vec -> vec -> vec

  val add_mat : mat -> mat -> mat
end



module FloatExtended (M : MATRIX_ARG) : (MATRIX with type value = M.value)= 
struct
  type value = M.value
  type vec = value array
  type mat = vec array

  let string_of_val = M.string_of_val

  let val_of_string = M.val_of_string

  let float_of_val = M.float_of_val

  let val_of_float = M.val_of_float

  let zero_value = M.zero_value

  let big_test_matrix = M.big_test_matrix

  (* Generates a vector of 0's with length len. *)
  let zero_vec (len : int) : vec = Array.create ~len M.zero_value

  (* Generates a matrix of 0's with the specified dimensions. *)
  let zero_mat (cols : int) (rows : int) : mat =
    Array.create ~len:cols (Array.create ~len:rows M.zero_value)

  (* Constructs the nth basis vector (zero-indexed) in R^dim. *)
  let basis_vec ~(dim : int) (n : int) : vec =
    if n >= dim then raise IndexOutOfBounds
    else
      let result : vec = Array.create ~len:dim M.zero_value in
      result.(n) <- (val_of_float 1.);
      result

  (* Generates the dim x dim identity matrix. *)
  let identity (dim : int) : mat =
    Array.init dim ~f:(fun i -> basis_vec ~dim i)

  (* Computes [|f l1.(0) l2.(0); f l1.(1); f l2.(1); ...|]. *)
  let map2 ~(f : value -> value -> 'a) (l1 : vec) (l2 : vec) : 'a array =
    let len1 = Array.length l1 in
    if len1 <> Array.length l2 then raise SizeMismatch
    else
      let result = Array.create ~len:len1 (f l1.(0) l2.(0)) in
      for i = 0 to len1 - 1 do
        result.(i) <- f l1.(i) l2.(i)
      done;
      result

  (* Adds two vectors. *)
  let add_vec : vec -> vec -> vec = map2 
    ~f:(fun x y -> M.val_of_float ((M.float_of_val x) +. (M.float_of_val y)))

  (* Adds two matrices. *)
  let add_mat (m1 : mat) (m2 : mat) : mat =
    let (len1, len2) = (Array.length m1, Array.length m2) in
    if len1 <> len2 then raise SizeMismatch
    else
      let result = zero_mat len1 (Array.length m1.(0)) in
      for i = 0 to len1 - 1 do
	result.(i) <- add_vec m1.(i) m2.(i)
      done;
      result
end
