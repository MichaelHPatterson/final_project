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

Random.self_init ();;

module type MATRIX =
sig
  (* type of elements in the matrix *)
  type value = float
  (* type for the vector *)
  type vec = value array
  (* type for the matrix - an array of columns *)
  type mat = vec array

  (* takes dimension as an int argument and returns 0 vec of that dim *)
  val zero_vec : int -> vec

  (* takes number of columns as first arg, number of rows as second arg, and 
   * returns 0 matrix of those dimensions *)
  val zero_mat : int -> int -> mat

  (* Constructs the nth standard basis vector (zero-indexed) in R^dim, where
   * dim is the first int argument and n is the second *)
  val basis_vec : dim:int -> int -> vec

  (* takes one argument for dimension and returns identity matrix *)
  val identity : int -> mat

  (* performs specified operation on two vectors, returning another vector *)
  val map2 : f:(value -> value -> 'a) -> vec -> vec -> 'a array
	 
  (* adds two vectors together *)
  val add_vec : vec -> vec -> vec

  (* adds two same-dimension matrices together *)
  val add_mat : mat -> mat -> mat

  (* returns the scalar multiple of a vector *)
  val scalar_mult_vec : vec -> float -> vec

  (* returns the scalar multiple of a matrix *)
  val scalar_mult_mat : mat -> float -> mat

  (* Multiplies a square matrix m with a vector v. *)
  val mult_vec : mat -> vec -> vec

  (* Multiplies two square matrices in order *)
  val mult_mat : mat -> mat -> mat

  (* Takes the tranpose of the matrix, making their columns into rows *)
  val transpose : mat -> mat
 
  (* Swaps two columns, denoted by int arguments, in the matrix *)
  val swap : mat -> int -> int -> unit
end


module FloatOps : MATRIX = 
struct
  type value = float
  type vec = value array
  type mat = vec array

  (* Generates a vector of 0's with length len. *)
  let zero_vec (len : int) : vec = Array.create ~len 0.

  (* Generates a matrix of 0's with the specified dimensions. *)
  let zero_mat (cols : int) (rows : int) : mat =
    Array.create ~len:cols (Array.create ~len:rows 0.)

  (* Constructs the nth basis vector (zero-indexed) in R^dim. *)
  let basis_vec ~(dim : int) (n : int) : vec =
    if n >= dim then raise IndexOutOfBounds
    else
      let result : vec = Array.create ~len:dim 0. in
      result.(n) <- 1.;
      result

  (* Generates the dim x dim identity matrix. *)
  let identity (dim : int) : mat =
    Array.init dim ~f:(fun i -> basis_vec ~dim i)

  (* Computes [|f l1.(0) l2.(0); f l1.(1); f l2.(1); ...|]. *)
  let map2 ~(f : float -> float -> 'a) (l1 : vec) (l2 : vec) : 'a array =
    let len1 = Array.length l1 in
    if len1 <> Array.length l2 then raise SizeMismatch
    else
      let result = Array.create ~len:len1 (f l1.(0) l2.(0)) in
      for i = 0 to len1 - 1 do
        result.(i) <- f l1.(i) l2.(i)
      done;
      result

  (* Adds two vectors. *)
  let add_vec : vec -> vec -> vec = map2 ~f:(+.)

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

  (* Multiplies the vector v by a scalar value. *)
  let scalar_mult_vec (v : vec) (factor : float) : vec =
    Array.map ~f:(( *. ) factor) v

  (* Multiplies the matrix m by a scalar value. *)
  let scalar_mult_mat (m : mat) (factor : float) : mat =
    Array.map ~f:(fun v -> scalar_mult_vec v factor) m

  (* Adds coeff * the identity matrix to m. *)
  let add_identity (m : mat) (coeff : float) : mat =
    let i = scalar_mult_mat (identity (Array.length m)) coeff in
    add_mat m i

  (* Multiplies a square matrix m with a vector v. Interprets each sub-array in
   * m as a column of m. *)
  let mult_vec (m : mat) (v : vec) : vec =
    let len = Array.length m in
    if len <> Array.length v then raise SizeMismatch
    else
      let result : vec ref = ref (zero_vec len) in
      for i = 0 to len - 1 do
        result := add_vec !result (scalar_mult_vec m.(i) v.(i))
      done;
      !result

  (* Multiplies two square matrices m1 and m2, in that order *)
  let mult_mat (m1 : mat) (m2 : mat) : mat =
    let (cols1, cols2) = (Array.length m1, Array.length m2) in
    if cols1 <> cols2 then raise SizeMismatch
    else if cols1 = 0 then [||]
    else
      let (rows1, rows2) = (Array.length m1.(0), Array.length m2.(0)) in
      if rows1 <> rows2 || rows1 <> cols1 then raise SizeMismatch
      else
	let result = zero_mat cols1 rows1 in
	for i = 0 to cols2 - 1 do
	  result.(i) <- mult_vec m1 m2.(i)
	done;
	result

  (* Computes the transpose of the matrix m. *)
  let transpose (m : mat) : mat =
    let num_cols = Array.length m in
    if num_cols = 0 then m
    else
      let num_rows = Array.length m.(0) in
      let result = Array.make_matrix num_rows num_cols 0. in
      for i = 0 to num_rows - 1 do
        let v : vec = Array.create ~len:num_cols 0. in
        for j = 0 to num_cols - 1 do
	  v.(j) <- m.(j).(i)
        done;
        result.(i) <- v
      done;
      result

  (* Swaps columns n1 and n2 in the matrix m. *)
  let swap (m : mat) (n1 : int) (n2 : int) : unit =
    let len = Array.length m in
    if n1 >= len || n2 >= len then raise IndexOutOfBounds
    else if n1 = n2 then ()
    else
      let v = m.(n1) in
      m.(n1) <- m.(n2);
      m.(n2) <- v
end
