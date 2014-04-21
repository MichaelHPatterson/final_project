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

Random.self_init ();;

module type MATRIX =
sig
  (* type of elements in the matrix *)
  type value
  (* type for the vector *)
  type vec
  (* type for the matrix*)
  type mat

  (* takes dimension as an int argument and returns 0 vec of that dim *)
  val zero_vec : int -> vec

  (* takes number of columns as first arg, number of rows as second arg, and 
   * returns 0 matrix of those dimensions *)
  val zero_mat : int -> int -> mat

  (* Constructs the nth standard basis vector (zero-indexed) in R^dim, where
   * dim is the first int argument and n is the second *)
  val basis_vec : int -> int -> vec

  (* takes one argument for dimension and returns identity matrix *)
  val identity : int -> mat

  (* performs specified operation on two vectors, returning another vector *)
  val map2 : (value -> value -> value) -> vec -> vec -> vec
	 
  (* adds two vectors together *)
  val add_vec : vec -> vec -> vec

  (* adds two same-dimension matrices together *)
  val add_mat : mat -> mat -> mat

  (* returns the scalar multiple of a vector *)
  val scalar_mult_vec : vec -> value -> vec

  (* returns the scalar multiple of a matrix *)
  val scalar_mult_mat : mat -> value -> mat

  (* Multiplies a square matrix m with a vector v. *)
  val mult_vec : mat -> vec -> vec

  (* Multiplies two square matrices in order *)
  val mult_mat : mat - mat -> mat

  (* Takes the tranpose of the matrix, making their columns into rows *)
  val transpose : mat -> mat
 
  (* Swaps two columns, denoted by int arguments, in the matrix *)
  val swap : mat -> int -> int -> unit

  (* Performs row-reduction on a matrix, returning a mat * int tuple because
   * the int keeps track of pivotal columns used in eigenvalue calculation *)
  val row_reduce : mat -> (mat * int)

  (* Prints vectors or matrices to the command line *)
  val print_vec : vec -> unit
  val print_mat : mat -> unit

  (* Returns the inverse of matrix, or None if it is not invertible *)
  val inverse : mat -> mat option

  (* Finds the eigensystem of a matrix, defined as an array of float-vector
   * tuples. *)
  val eigen : mat -> (float * vec) array

  (* calculates e to a certain level of precision, given by an int *)
  val e : int -> float
  
  (* calculates the matrix exponential of a given matrix *)
  val exponentiate : mat -> mat

  (* diagonalizes a matrix, returning a tuple of a matrix of an orthnormal
     eigenbasis and a diagonal matrix of eigenvalues *)
  val diagonalize : mat -> (mat * mat)
end
								  
(* Note: this is just preliminary and could require some modifications. *)
module FloatMatrix =
struct
  type vec = float array
  (* matrix = array of columns *)
  type mat = vec array

  

  let zero_vec (len : int) : vec = Array.create ~len 0.

  let zero_mat (cols : int) (rows : int) : mat =
    Array.create ~len:cols (Array.create ~len:rows 0.)

  (* Constructs the nth basis vector (zero-indexed) in R^dim. *)
  let basis_vec ~(dim : int) (n : int) : vec =
    if n >= dim then raise IndexOutOfBounds
    else
      let result : vec = Array.create ~len:dim 0. in
      result.(n) <- 1.;
      result

  let identity (dim : int) : mat =
    Array.init dim ~f:(fun i -> basis_vec ~dim i)

  let map2 ~(f : float -> float -> 'a) (l1 : vec) (l2 : vec) : 'a array =
    let len1 = Array.length l1 in
    if len1 <> Array.length l2 then raise SizeMismatch
    else
      let result = Array.create ~len:len1 (f l1.(0) l2.(0)) in
      for i = 0 to len1 - 1 do
        result.(i) <- f l1.(i) l2.(i)
      done;
      result

  let add_vec : vec -> vec -> vec = map2 ~f:(+.)

  let add_mat (m1 : mat) (m2 : mat) : mat =
    let (len1, len2) = (Array.length m1, Array.length m2) in
    if len1 <> len2 then raise SizeMismatch
    else
      let result = zero_mat len1 (Array.length m1.(0)) in
      for i = 0 to len1 - 1 do
	result.(i) <- add_vec m1.(i) m2.(i)
      done;
      result

  let scalar_mult_vec (v : vec) (factor : float) : vec =
    Array.map ~f:(( *. ) factor) v

  let scalar_mult_mat (m : mat) (factor : float) : mat =
    Array.map ~f:(fun v -> scalar_mult_vec v factor) m

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

  let swap (m : mat) (n1 : int) (n2 : int) : unit =
    let len = Array.length m in
    if n1 >= len || n2 >= len then raise IndexOutOfBounds
    else if n1 = n2 then ()
    else
      let v = m.(n1) in
      m.(n1) <- m.(n2);
      m.(n2) <- v

  (* Row_reduces a matrix. Seems to be working properly, but further testing
   * couldn't hurt. Based on an algorithm I learned in math, and the steps of
   * that algorithm are copy-pasted in as comments. *)
  let row_reduce (m : mat) : mat * int =
    (* Column reduces rows n through Array.length m - 1 of m. Assumes that <piv>
     * pivotal 1's have been generated so far. *)
    let rec col_reduce_past (m : mat) (n : int) (piv : int) : mat * int =
      let len = Array.length m in
      if len = 0 then (m, piv)
      else let height = Array.length m.(0) in
      if n >= len || n >= height || piv >= len || piv >= height then (m, piv) else
      (* (a) SWAP rows so that the leftmost column that is not all zeroes has a
             nonzero entry in the first row. *)
      (* Returns the column index of the first nonzero element in the specified
       * row <row>, ignoring the first <piv> elements. *)
      let first_nonzero (m : mat) (row : int) : (int * float) option =
	let len = Array.length m in
        let ans = ref (len, 0.) in
        for i = len - 1 downto piv do
	  if m.(i).(row) <> 0. then ans := (i, m.(i).(row))
	done;
        let (c, _) = !ans in if c = len then None else Some !ans
      in
      match first_nonzero m n with
      | None ->
        (* No pivotal 1 found in that row. *)
        col_reduce_past m (n + 1) piv
      | Some (swap_col, value) ->
        (* Pivotal value of <value> found in <swap_col>th position. *)
	swap m n swap_col;

        (* (b) DIVIDE by this entry to get a pivotal 1. *)
        m.(n) <- scalar_mult_vec m.(n) (1. /. value);

        (* (c) SUBTRACT multiples of the first row from the others to clear out the
            rest of the column under the pivotal 1. *)
        for i = 0 to len - 1 do
	  if i <> n then m.(i) <- add_vec m.(i) (scalar_mult_vec m.(n) (-1. *. m.(i).(n)))
	done;

	col_reduce_past m (n + 1) (piv + 1)

    in let (m,p) = col_reduce_past (transpose m) 0 0 in (transpose m, p)


  let print_vec (v : vec) : unit =
    let len = Array.length v in
    if len = 0 then ()
    else 
      Printf.printf "[|%f" v.(0);
      for i = 1 to len - 1 do
	Printf.printf "; %f" v.(i)
      done;
      Printf.printf "|]";
      flush_all ()

  let print_mat (m : mat) : unit =
    Array.iter ~f:(fun v -> print_vec v; Printf.printf "\n") (transpose m)

  (* Inverts a square matrix with the help of row-reduction. *)
  let inverse (m : mat) : mat option =
    let width = Array.length m in
    let height = Array.length m.(0) in
    if width <> height then raise NotSquare
    else
      let new_mat : mat = let (m,_) = row_reduce (Array.append m (identity width)) in m in
      let is_identity = ref true in
      for i = 0 to width - 1 do
	for j = 0 to height - 1 do
	  if i = j then
	    (if Float.abs (new_mat.(i).(j) -. 1.) > 0.001 then is_identity := false)
	  else if Float.abs new_mat.(i).(j) > 0.001 then is_identity := false
	done;
      done;
      if not !is_identity then None
      else
	let result : mat = zero_mat width height in
	for i = 0 to width - 1 do
	  result.(i) <- new_mat.(i + width)
	done;
	Some result


  (* This function should ideally compute both eigenvalues and the corresponding
   * eigenvectors simultaneously (i.e. the return type should be something like
   * "(float * vec) list"). That shouldn't be very hard to add in. *)
  (* Note: The call to Polynomial.newton_all_slow doesn't work with C-c C-e; it
   * requires compilation from the terminal. *)
  (* Note: I'm not sure if this works with matrices that have repeated
   * eigenvalues but still have an eigenbasis (e.g. it doesn't work for identity
   * matrices). *)
  let eigen (m : mat) : (float * vec) array =
    let dim = Array.length m in
    let start : vec = zero_vec dim in
    Array.iteri ~f:(fun i _ -> start.(i) <- Random.float 100. +. 5.) start;
    let new_vec = ref start in
    let new_mat = zero_mat (dim + 1) dim in
    new_mat.(0) <- !new_vec;
    for i = 0 to dim - 1 do
      new_vec := mult_vec m !new_vec;
      new_mat.(i+1) <- !new_vec
    done;
    let p =
      Array.append (scalar_mult_vec (let (m,_) = row_reduce new_mat in m).(dim) (-1.)) [|1.|] in
    let eigenvalues = List.to_array (Polynomial.newton_all_slow p (-100., 100.) 1. 0.01 0.0001) in
    let f (index : int) (e : float) : float * vec =
      let v = ref start in
      for j = 0 to Array.length eigenvalues - 1 do
	if index <> j then
	  let matrix = add_mat m (scalar_mult_mat (identity dim) ((-1.) *. eigenvalues.(j))) in
	  v := mult_vec matrix !v
      done;
      (e, !v)
    in
    Array.mapi ~f eigenvalues

  (* This version of the function is slower (it row-reduces after every
   * application of the matrix instead of applying the matrix n times first).
   * It might be necessary to prevent a corner case in which the starting
   * vector is an eigenvector of m. However, it's probably better to just use
   * the version above, because: (1) given the infinitude of the real numbers,
   * the probability of that corner case should be 0, and (2) I'm pretty sure
   * the other version would still work with that corner case. *)
  let eigen_new (m : mat) : (float * vec) array =
    let dim = Array.length m in
    let rec find (start_vec : vec) (v : vec) (curr : mat) : (float * vec) list =
      let next_vec = mult_vec m v in
      let curr = Array.append curr [|next_vec|] in
      let size = Array.length curr in
      let (test_mat, piv) = row_reduce curr in
      if size = piv then find start_vec next_vec curr
      else
	let p =
	  if size > dim then Array.append test_mat.(size - 1) [|-1.|]
          else let _ = test_mat.(size - 1).(size - 1) <- (-1.) in test_mat.(size - 1)
        in
	Polynomial.print_poly p; Printf.printf "\n"; flush_all ();						       
	let eigenvalues = List.to_array (Polynomial.newton_all_slow p (-100., 100.) 1. 0.01 0.0001) in
	Printf.printf "Found eigenvalues\n"; flush_all ();
	let f (index : int) (e : float) : float * vec =
	  let v = ref start_vec in
	  for j = 0 to Array.length eigenvalues - 1 do
	    if index <> j then
	      let matrix = add_mat m (scalar_mult_mat (identity dim) ((-1.) *. eigenvalues.(j))) in
	      v := mult_vec matrix !v
	  done;
	  (e, !v)
	in Array.to_list (Array.mapi ~f eigenvalues)
      in
    let gen_vec () : vec =
      let start : vec = zero_vec dim in
      Array.iteri ~f:(fun i _ -> start.(i) <- Random.float 100. +. 5.) start;
      start
    in
    let rec check_repeat (already_found : int) : (float * vec) list =
      let start = gen_vec () in
      let e = find start start [||] in
      let num_found = List.length e + already_found in
      if num_found < dim then e @ (check_repeat num_found)
      else e
    in
    List.to_array (check_repeat 0)

  (* Estimates e by summing 1/i! from i=0 to i=precision. This could also be
   * done by evaluating (1+1/k)^k for large k, but k would have to be really
   * large for that to be accurate, so I think this might be faster. I'm not
   * totally sure. *)
  let e (precision : int) : float =
    let rec factorial (x : int) : int =
      if x <= 1 then 1
      else x * factorial (x - 1)
    in
    let rec e_rec (from : int) : float =
      if from > precision then 0.
      else 1. /. (float (factorial from)) +. e_rec (from + 1)
    in e_rec 0

  (* This is still in development -- e.g. it has an incomplete match statement at the bottom. *)
  let exponentiate (m : mat) : mat =
    let dim = Array.length m in
    let eigenbasis = zero_mat dim dim in
    let diagonal = zero_mat dim dim in
    let f i (value, vector) : unit =
      diagonal.(i) <- scalar_mult_vec (basis_vec ~dim i) ((e 15) ** value);
      eigenbasis.(i) <- vector
    in
    Array.iteri ~f (eigen m);
    mult_mat eigenbasis (mult_mat diagonal (let Some inv = inverse eigenbasis in inv))
end

(* For typing convenience *)
module M = FloatMatrix

(* Verifying that the 5x5 identity matrix is invertible *)
let m1 = M.identity 5 in
match M.inverse m1 with
| None -> Printf.printf "The matrix is not invertible.\n\n\n"
| Some m -> M.print_mat m; Printf.printf "\n\n"
;;

(* A more difficult invertible matrix *)
let m2 = [|[|0.127131; 0.873108; 0.116526; 0.452341|]; [|0.405467; 0.91256; 0.0373603; 0.50571|]; [|0.703187; 0.126679; 0.537015; 0.710102|]; [|0.964194; 0.052814; 0.731034; 0.103877|]|] in
(match M.inverse m2 with
| None -> Printf.printf "The matrix is not invertible.\n\n"
| Some m -> M.print_mat m; Printf.printf "\n");
M.print_mat (M.exponentiate m2); Printf.printf "\n\n\n"
;;

(* A non-invertible matrix *)
let m3 = [|[|1.; 0.; 0.; 0.; 0.|]; [|0.; 1.; 0.; 0.; 0.|]; [|0.; 0.; 1.; 0.; 0.|]; [|0.; 0.; 1.; 0.; 0.|]; [|0.; 0.; 0.; 0.; 1.|]|] in
match M.inverse m3 with
| None -> Printf.printf "The matrix is not invertible.\n\n\n"
| Some m -> M.print_mat m; Printf.printf "\n\n"
;;

(* Simple invertible test matrix, with eigenvalues -1, 4, and 7 *)
let m4 = [|[|4.;0.;0.|]; [|-2.;2.;-5.|]; [|4.5;-3.;4.|]|] in
let v = M.eigen m4 in
let f (value, vector) =
  Printf.printf "Eigenvalue %f corresponds to eigenvector " value;
  M.print_vec vector;
  Printf.printf "\n"
in
Array.iter ~f v;
Printf.printf "\n";
(match M.inverse m4 with
| None -> Printf.printf "The matrix is not invertible.\n\n"
| Some m -> M.print_mat m; Printf.printf "\n");
M.print_mat (M.exponentiate m4); Printf.printf "\n"
;;
