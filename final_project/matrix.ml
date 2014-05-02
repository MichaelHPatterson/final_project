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
  type value = float
  type vec = value array
  type mat = vec array

  (* Generates a vector of 0's with length len. *)
  let zero_vec (len : int) : vec = Array.create ~len 0.

  (* Generates a matrix of 0's with the specified dimensions. *)
  let zero_mat (dimx : int) (dimy : int) : mat =
    Array.make_matrix ~dimx ~dimy 0.

  (* Checks whether v is the zero vector. *)
  let is_zero_vec (v : vec) (precision : float) : bool =
    Array.for_all v ~f:(fun x -> Float.abs x <= precision)

  (* Computes the transpose of the matrix m. *)
  let transpose (m : mat) : mat =
    let num_cols = Array.length m in
    if num_cols = 0 then m
    else
      let num_rows = Array.length m.(0) in
      let result = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0. in
      for i = 0 to num_rows - 1 do
        let v : vec = Array.create ~len:num_cols 0. in
        for j = 0 to num_cols - 1 do
	  v.(j) <- m.(j).(i)
        done;
        result.(i) <- v
      done;
      result

  (* Prints out a vector. *)
  let print_vec (v : vec) : unit =
    let len = Array.length v in
    if len = 0 then ()
    else 
      Printf.printf "{%f" v.(0);
      for i = 1 to len - 1 do
	Printf.printf ", %f" v.(i)
      done;
      Printf.printf "}";
      flush_all ()

  (* Prints out a matrix. *)
  let print_mat (m : mat) : unit =
    Printf.printf "{";
    Array.iter ~f:(fun v -> print_vec v; Printf.printf ",\n") (transpose m);
    Printf.printf "}\n"

  (* Multiplies the vector v by a scalar value. *)
  let scalar_mult_vec (v : vec) (factor : float) : vec =
    Array.map ~f:(( *. ) factor) v

  (* Multiplies the matrix m by a scalar value. *)
  let scalar_mult_mat (m : mat) (factor : float) : mat =
    Array.map ~f:(fun v -> scalar_mult_vec v factor) m

  let equal_vec (v1 : vec) (v2 : vec) (precision : float) : bool =
    let f a b = Float.abs (a -. b) <= precision in
    try (Array.for_all2_exn ~f v1 v2)
    with (Invalid_argument _) -> raise SizeMismatch

  let equal_mat (m1 : mat) (m2 : mat) (precision : float) : bool =
    try (Array.for_all2_exn ~f:(fun v1 v2 -> equal_vec v1 v2 precision) m1 m2)
    with (Invalid_argument _) -> raise SizeMismatch

  (* Checks whether v1 is a scalar multiple of v2. The 3rd argument is the
   * allowed percent error. *)
  let is_mult (v1 : vec) (v2 : vec) (perc : float) : bool =
    let ratios : float array =
      try (Array.map2_exn ~f:(/.) v1 v2)
      with (Invalid_argument _) -> raise SizeMismatch
    in
    if Array.length ratios = 0 then true
    else
      let first_non_nan : float =
	let f a x = if not (Float.is_nan a) || Float.is_nan x then a else x in
	Array.fold ~f ~init:Float.nan ratios in
      let v1' = scalar_mult_vec v2 first_non_nan in
      let min_val = Array.fold ~f:Float.min ~init:v1'.(0) v1' in
      equal_vec v1 (scalar_mult_vec v2 first_non_nan) (perc *. min_val)
      (* Array.for_all ~f:(fun r -> first_non_nan -. r <= precision || Float.is_nan r) ratios *)

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

  (* Swaps columns n1 and n2 in the matrix m. *)
  let swap (m : mat) (n1 : int) (n2 : int) : unit =
    let len = Array.length m in
    if n1 >= len || n2 >= len then raise IndexOutOfBounds
    else if n1 = n2 then ()
    else
      let v = m.(n1) in
      m.(n1) <- m.(n2);
      m.(n2) <- v

  (* Row_reduces a matrix. Seems to be working properly, but further testing
   * couldn't hurt. The steps of the algorithm, from my math class, are pasted
   * in as comments. *)
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

  (* Checks whether the first n columns of m are the identity matrix. *)
  let is_identity (m : mat) (n : int) (precision : float) : bool =
    let m =
      try (Array.sub m ~pos:0 ~len:n)
      with (Invalid_argument _) -> raise IndexOutOfBounds in
    let id_tracker = ref true in
    let _ = try
    (for i = 0 to n - 1 do
      if Array.length m.(i) <> n then raise NotSquare;
      for j = 0 to n - 1 do
	if i = j then
	  (if Float.abs (m.(i).(j) -. 1.) > precision then id_tracker := false)
	else if Float.abs m.(i).(j) > precision then id_tracker := false
      done
    done)
    (* Given a properly constructed matrix, the only way an index could go out
     * of bounds in the statements above is if m is not square. *)
    with (Invalid_argument "index out of bounds") -> raise NotSquare
    in !id_tracker

  (* Inverts a square matrix with the help of row-reduction. *)
  let inverse (m : mat) : mat option =
    let width = Array.length m in
    let height = Array.length m.(0) in
    if width <> height then raise NotSquare
    else
      let new_mat : mat = let (m,_) = row_reduce (Array.append m (identity width)) in m in
      if not (is_identity new_mat width 0.001) then None
      else
	let result : mat = zero_mat width height in
	for i = 0 to width - 1 do
	  result.(i) <- new_mat.(i + width)
	done;
	Some result

  (* Returns the eigenvalues and eigenvectors of a matrix m. *)
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
    let eigenvalues = List.to_array (Polynomial.newton_all_slow p (-1000., 1000.) 1. 0.01 0.0001) in
    let f (index : int) (e : float) : float * vec =
      let v = ref start in
      for j = 0 to Array.length eigenvalues - 1 do
	if index <> j then
	  let matrix = add_identity m ((-1.) *. eigenvalues.(j)) in
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
    let rec find (start_vec : vec) (v : vec) (curr : mat) ((l,u) : float * float) : (float * vec) list =
      let next_vec = mult_vec m v in
      let curr = Array.append curr [|next_vec|] in
      print_mat curr; Printf.printf "\n";
      let size = Array.length curr in
      let (test_mat, piv) = row_reduce curr in
      if size = piv then find start_vec next_vec curr (l,u)
      else
	let p =
	  if size > dim then Array.append test_mat.(size - 1) [|-1.|]
          else let _ = test_mat.(size - 1).(size - 1) <- (-1.) in test_mat.(size - 1)
        in
	Printf.printf "Analyzing polynomial: "; print_vec p; Printf.printf "\n";
	let eigenvalues = List.to_array (Polynomial.newton_all_slow p (l,u) 0.1 0.01 0.00001) in
	Printf.printf "Found %i eigenvalues:" (Array.length eigenvalues);
	Array.iter ~f:(fun e -> Printf.printf " %f" e) eigenvalues; Printf.printf "\n";
	let f (index : int) (e : float) : float * vec =
	  let v = ref start_vec in
	  for j = 0 to Array.length eigenvalues - 1 do
	    if j <> index then
	      let matrix = add_identity m ((-1.) *. eigenvalues.(j)) in
	      v := mult_vec matrix !v
	  done;
	  (e, !v)
	in let a = Array.to_list (Array.filter ~f:(fun (e,v) -> let b = not (is_zero_vec v 0.00001) in if b then true else (Printf.printf "The following vector, with eigenvalue %f, is the zero vector and is being excluded: " e; print_vec v; Printf.printf "\n"; flush_all (); false)) (Array.mapi ~f eigenvalues)) in (Printf.printf "Length: %i\n" (List.length a); a)
      in
    let gen_vec () : vec =
      let start : vec = zero_vec dim in
      Array.iteri ~f:(fun i _ -> start.(i) <- Random.float 10. +. 5.) start;
      start
    in
    let rec find_all (already_found : (float * vec) list) ((l,u) : float * float) : (float * vec) list =
      let start = gen_vec () in
      Printf.printf "\nStarting with vector: "; print_vec start; Printf.printf "\n\n";
      let remove_repeats (evs : (float * vec) list) : (float * vec) list =
	let f lst (e,v) =
	  (e,v) :: (List.filter ~f:(fun (_,v') -> let b = not (is_mult v v' 0.01) in if b then true else (Printf.printf "The vector: "; print_vec v; Printf.printf "\nis a scalar multiple of the vector: "; print_vec v'; Printf.printf "\n"; false)) lst)
	in List.fold_left ~f ~init:[] evs
      in
      let evs = find start start [|start|] (l,u) @ already_found in
      Printf.printf "WITHOUT REMOVING REPEATS:\n";
      List.iter ~f:(fun (e,v) -> Printf.printf "Eigenvalue %f ===> Eigenvector " e; print_vec v; Printf.printf "\n") evs;
      Printf.printf "\n";
      let evs = remove_repeats evs in
      Printf.printf "AFTER REMOVING REPEATS:\n";
      List.iter ~f:(fun (e,v) -> Printf.printf "Eigenvalue %f ===> Eigenvector " e; print_vec v; Printf.printf "\n") evs;
      Printf.printf "\n"; flush_all ();
      let num_found = List.length evs in
      Printf.printf "Have found a total of %i so far.\n" num_found; flush_all (); Thread.delay (if dim > 3 then 1. else 0.5);
      if num_found < dim then find_all evs (l,u)
      else (Printf.printf "Done!\n"; List.iter ~f:(fun (_,v) -> print_vec v; Printf.printf "\n") evs; flush_all (); (*Thread.delay 1.; *)evs)
    in
    let rec call_finder ((l,u) : float * float) : (float * vec) list =
      try (try (
	let e = find_all [] (l,u) in
	if List.length e <> dim then call_finder (2. *. l, 2. *. u)
        else
	  let p : mat ref = ref [||] in
	  let d : mat ref = ref [||] in
	  let n : int ref = ref 0 in
	  List.iter ~f:(fun (a,v) -> p := Array.append !p [|v|]; d := Array.append !d [|scalar_mult_vec (basis_vec ~dim !n) a|]; n := !n + 1) e;
	  let m' =
	    match inverse !p with
	    | None -> raise InversionError
	    | Some matrix -> mult_mat (mult_mat !p !d) matrix in
	  if (let b = equal_mat m m' 0.1 in if b then true else let _ = (Printf.printf "Function screwed up!\n"; flush_all (); print_mat m') in false) then e
	  else call_finder (2. *. l, 2. *. u)
(*	if List.for_all ~f:(fun (_,v) -> let b = is_mult (mult_vec m v) v 0.5 in if b then true else (let _ = (Printf.printf "Function screwed up!\nVector: "; print_vec v; Printf.printf "\nMultiplied by matrix: "; print_vec (mult_vec m v); Printf.printf "\n"; flush_all ()) in false)) e then e
	else call_finder (2. *. l, 2. *. u)*)
      )
      with IndexOutOfBounds -> (Printf.printf "Index out of bounds!\n"; flush_all (); call_finder (2. *. l, 2. *. u)))
      with InversionError -> (Printf.printf "Algorithm error!\n"; flush_all (); call_finder (2. *. l, 2. *. u)) in
    Printf.printf "Analyzing the matrix:\n"; print_mat m; flush_all ();
    List.to_array (call_finder (-100.,100.))

  (* Estimates e by summing 1/i! from i=0 to i=precision. This could also be
   * done by evaluating (1+1/k)^k for large k, but k would have to be really
   * large for that to be accurate, so I think this might be faster. I'm not
   * totally sure. *)
  let e (precision : int) : float =
    let rec factorial (x : int) : int =
      if x <= 1 then 1
      else x * factorial (x - 1)
    in
    let rec e_rec (from : int) (curr : float) : float =
      if from > precision then curr
      else e_rec (from + 1) (curr +. (1. /. (float (factorial from))))
    in e_rec 0 0.

  (* This is still in development -- e.g. it has an incomplete match statement at the bottom. *)
  let exponentiate (m : mat) : mat =
    let dim = Array.length m in
    let eigenbasis = zero_mat dim dim in
    let diagonal = zero_mat dim dim in
    let f i (value, vector) : unit =
      Printf.printf "Dimension: %i. Constructing %ith standard basis vector.\n" dim i;
      diagonal.(i) <- scalar_mult_vec (basis_vec ~dim i) ((e 15) ** value);
      eigenbasis.(i) <- vector
    in
    let g (value, vector) =
      Printf.printf "Eigenvalue %f corresponds to eigenvector " value;
      print_vec vector;
      Printf.printf "\n"
    in
    let es = eigen_new m in
    Array.iter ~f:g es;
    Printf.printf "\n";
    Array.iteri ~f es;
    Printf.printf "Matrix of eigenvectors:\n"; print_mat eigenbasis; Printf.printf "\n";
    let inv = match inverse eigenbasis with
      | None -> raise InversionError
      | Some matrix -> matrix
    in mult_mat eigenbasis (mult_mat diagonal inv)

  (*let exponentiate2 (m : mat) : mat =
    let dim = Array.length m in
    let prec = Float.to_int (float dim *. 1.5) + 1 in
    let mat_pow (m : mat) (p : int) =
      let rec mat_pow_helper (curr : mat) (m : mat) (p : int) =
	if p <= 0 then curr
	else mat_pow_helper (mult_mat curr m) m (p - 1)
      in mat_pow_helper (identity dim) m p
    in
    let rec factorial (n : int) : float =
      if n <= 0 then 1.
      else (float n) *. factorial (n - 1) in
    let rec exp_helper (m : mat) (prec : int) (curr : mat) : mat =
      if prec < 0 then curr
      else
	let next_term = scalar_mult_mat (mat_pow m prec) (1. /. (factorial prec)) in
	if prec = dim || prec = Float.to_int (1.5 *. (float dim)) then
	  (Printf.printf "\nNow adding m^%i/%i!, which is:\n" prec prec;
	   print_mat next_term);
	exp_helper m (prec - 1) (add_mat curr next_term)
    in exp_helper m prec (zero_mat dim dim)*)

  let exponentiate2 (m : mat) : mat =
    let dim = Array.length m in
    let stop = dim * 2 in
    let rec exp_helper (curr : mat) (last : mat) (pos : int) : mat =
      if pos > stop then curr
      else
	let next = scalar_mult_mat (mult_mat m last) (1. /. float pos) in
	exp_helper (add_mat curr next) next (pos + 1) in
    let i = identity dim in
    exp_helper i i 1

  (* Takes a matrix m of rankings, and computes m^T * m (where m^T is the
   * transpose of m), which is the matrix of element relationships. *)
  let rank_to_relations (m : mat) : mat =
    mult_mat (transpose m) m
end

(* For typing convenience *)
module M = FloatMatrix

let eigen_print (m : M.mat) : unit =
  let f (value, vector) =
    Printf.printf "Eigenvalue %f corresponds to eigenvector " value;
    M.print_vec vector;
    Printf.printf "\n"
  in
  Array.iter ~f (M.eigen_new m);
  Printf.printf "\n"

let inv_print (m : M.mat) : unit =
  match M.inverse m with
  | None -> Printf.printf "The matrix is not invertible.\n\n"
  | Some m' -> Printf.printf "Inverse:\n"; M.print_mat m'; Printf.printf "\n"

(*
(* Verifying that the 5x5 identity matrix has an eigenbasis and is invertible *)
let m1 = M.identity 5 in
eigen_print m1;
inv_print m1;
M.print_mat (M.exponentiate m1); Printf.printf "\n"
;;

(* A more difficult invertible matrix *)
let m2 = [|[|0.127131; 0.873108; 0.116526; 0.452341|]; [|0.405467; 0.91256; 0.0373603; 0.50571|]; [|0.703187; 0.126679; 0.537015; 0.710102|]; [|0.964194; 0.052814; 0.731034; 0.103877|]|] in
inv_print m2;
M.print_mat (M.exponentiate m2); Printf.printf "\n\n\n"
;;

(* A non-invertible matrix *)
inv_print [|[|1.; 0.; 0.; 0.; 0.|]; [|0.; 1.; 0.; 0.; 0.|]; [|0.; 0.; 1.; 0.; 0.|]; [|0.; 0.; 1.; 0.; 0.|]; [|0.; 0.; 0.; 0.; 1.|]|]
;;

(* Simple invertible test matrix, with eigenvalues -1, 4, and 7 *)
let m4 = [|[|4.;0.;0.|]; [|-2.;2.;-5.|]; [|4.5;-3.;4.|]|] in
eigen_print m4;
inv_print m4;
M.print_mat (M.exponentiate m4); Printf.printf "\n\n"
;;

(* More difficult matrix -- has eigenvalues 1, 2, and 2 *)
let m5 = [|[|2.;0.;0.|]; [|1.;2.;1.|]; [|-1.;0.;1.|]|] in
eigen_print m5;
inv_print m5;
M.print_mat (M.exponentiate m5); Printf.printf "\n"
;;

(* Another weird matrix, with eigenvalues 1, 1, and 3 *)
(* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: This example sometimes results in 1 or more of the following 5 problems: (1) eigen_new only finds 1 or 2 eigenvectors but not all 3; (2) eigen_new returns MORE than 3 vectors, and some vectors are scalar multiples ofe ach other; (3) eigen_new returns a zero vector; (4) the exponentiation function throws the InversionError exception (probably caused by problems #1, #2, and perhaps #3 above); and/or (5) the exponentiation function causes the basis_vec function to throw an IndexOutOfBounds exception *)
(* UPDATE: I think it's fixed now *)
let m6 = [|[|1.;0.;0.|]; [|0.;1.;0.|]; [|2.;0.;3.;|]|] in
eigen_print m6;
inv_print m6;
M.print_mat (M.exponentiate m6); Printf.printf "\n"
;;
  *)
