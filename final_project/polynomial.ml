(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors : Madhu Vijay & Michael Patterson
 * polynomial.ml -- Uses Newton's method to find the roots of a polynomial,
 * for the purposes of matrix eigendecomposition. *)

(* NOTE: This file is no longer used in our core exponentiation function. We
 * have included it anyway, since it took a lot of work and was critical to
 * our eigen function in matrix.ml. *)

open Core.Std

exception AlgorithmError

(* Type for polynomials; stores coefficients, starting with the constant value.
 * (Starting from the constant instead of the highest exponent ensures that the
 * index of each element equals the corresponding exponent.) *)
type poly = float array

(* Prints out the coefficients in a polynomial. *)
let print_poly (p : poly) : unit =
  Printf.printf "[|%f" p.(0);
  for i = 1 to Array.length p - 1 do
    Printf.printf "; %f" p.(i)
  done;
  Printf.printf "|]";
  flush_all ()

(* Evaluates a polynomial p at a value v. *)
let evaluate (p : poly) (v : float) : float =
  let vals : float array = Array.mapi ~f:(fun i x -> x *. (v ** float i)) p in
  Array.fold_right ~f:(+.) ~init:0. vals

(* Differentiates the polynomial p. *)
let derivative (p : poly) : poly =
  let p' : poly = Array.mapi ~f:(fun i x -> float i *. x) p in
  let len : int = Array.length p' - 1 in
  let ans : poly = Array.create ~len 0. in
  Array.blit ~src:p' ~src_pos:1 ~dst:ans ~dst_pos:0 ~len;
  ans

(* Applies Newton's method to find a root of p, given allowed imprecision
 * epsilon. *)
let rec newton (p : poly) (g : float) (epsilon : float) : float option =
  let abs_float : float -> float = fun x -> if x > 0. then x else (-.x) in
  if abs_float (evaluate p g) < epsilon then Some g
  else
    let deriv = evaluate (derivative p) g in
    if deriv = 0. then None
    else
      let g' = g -. (evaluate p g) /. deriv in
      newton p g' epsilon

(* Same as the newton function above, but assumes that you already know the
 * derivative (d) of p. *)
let rec newton_d (p : poly) (d : poly) (g : float) (epsilon : float)
	: float option =
  let abs_float : float -> float = fun x -> if x > 0. then x else (-.x) in
  if abs_float (evaluate p g) < epsilon then Some g
  else
    let deriv = evaluate d g in
    if deriv = 0. then None
    else 
      let g' = g -. (evaluate p g) /. deriv in
      newton_d p d g' epsilon

(* Applies Newton's method repeatedly to (theoretically) find all roots of p,
 * using epsilon = answer_prec. Starts with bounds lower and upper (though those
 * can expand if necessary). Spaces out guesses by try_prec. Discards repeat
 * solutions if they are within duplicate_prec of each other. *)
(* As explained in the documentation, this function does not always work *)
let rec newton_all (p : poly) ((lower, upper) : float * float)
			(try_prec : float) (duplicate_prec : float)
			(answer_prec : float) : float list =
  let rec newton_int_rec (d : poly) ((low, high) : float * float)
			 (curr : float list) : float list =
    if low > high then curr
    else
      let g = low in
      let root : float option = newton_d p d g answer_prec in
      let new_low = low +. try_prec in
      let new_list =
	match root with
	| None -> curr
	| Some root' ->
	  let f r = Float.abs (r -. root') < duplicate_prec in
          if List.exists ~f curr then curr else root' :: curr in
      newton_int_rec d (new_low, high) new_list
  in
  let solutions = newton_int_rec (derivative p) (lower, upper) [] in
  let f i c x = if Float.abs x > answer_prec then i else c in
  let degree = Array.foldi ~f ~init:0 p in
  let num_solutions = List.length solutions in
  match Int.compare degree num_solutions with
  | 1 -> let duplicate_prec = duplicate_prec /. 2. in
	 let answer_prec = answer_prec /. 2. in
	 newton_all p (lower,upper) try_prec duplicate_prec answer_prec
  | 0 -> solutions
  | _ -> let try_prec = try_prec *. 2. in
	 let duplicate_prec = duplicate_prec *. 2. in
	 let answer_prec = answer_prec /. 10. in
	 newton_all p (lower,upper) try_prec duplicate_prec answer_prec
