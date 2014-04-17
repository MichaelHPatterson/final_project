open Core.Std

(* Note to Mike: I'm not 100% sure this will be useful, but I do know that the
 * algorithm we learned in Math 23 for finding eigenvalues/eigenvectors relied 
 * on the ability to solve a polynomial equations, so I've tried to implement
 * that here. Hopefully it ends up being useful somehow. *)


(********************* TYPES AND OPERATIONS FOR POLYNOMIALS *********************)

(* Type for polynomials; stores coefficients, starting with the constant value.
 * (Starting from the constant instead of the highest exponent ensures that the
 * index of each element equals the corresponding exponent.) *)
type poly = float array

let rec evaluate (p : poly) (v : float) : float =
  let vals : float array = Array.mapi ~f:(fun i x -> x *. (v ** float i)) p in
  Array.fold_right ~f:(+.) ~init:0. vals

let derivative (p : poly) : poly =
  let p' : poly = Array.mapi ~f:(fun i x -> float i *. x) p in
  let len : int = Array.length p' - 1 in
  let ans : poly = Array.create ~len 0. in
  Array.blit p' 1 ans 0 len;
  ans

let rec newton (p : poly) (g : float) (epsilon : float) : float =
  let abs_float : float -> float = fun x -> if x > 0. then x else (-.x) in
  if abs_float (evaluate p g) < epsilon then g
  else
    let g' = g -. (evaluate p g) /. (evaluate (derivative p) g) in
    newton p g' epsilon

(* Fairly slow method of repeatedly applying Newton's method to find all roots
 * of p, between lower and upper. Increments each try by try_prec, and uses
 * answer_prec = epsilon. Ignores duplicates that are within duplicate_prec of
 * each other. *)
(* Note: This function is much more naive and slower than newton_all_fast below,
 * but if the precision values and lower/upper are right, newton_all_slow should
 * never fail to find all real roots between lower and upper, while the faster
 * function newton_all_fast might. *)
let newton_all_slow (p : poly) ((lower, upper) : float * float) (try_prec : float) (duplicate_prec : float) (answer_prec : float) : float list =
  let rec newton_int_rec ((low, high) : float * float) (curr : float list) : float list =
    if low > high then curr
    else
      let g = low in
      let root : float = newton p g answer_prec in
      let new_low = low +. try_prec in
      let new_list =
        let f = fun b r -> b || Float.abs (r -. root) < duplicate_prec in
        if List.fold_left ~f ~init:false curr then curr else root :: curr in
      newton_int_rec (new_low, high) new_list
  in newton_int_rec (lower, upper) []



(********************* TYPES AND OPERATIONS FOR INTERVALS *********************)

(* The intended purpose of this section was to generate a less naive way of
 * repeatedly applying Newton's method, by keeping track of an interval that
 * has already been checked. The function newton_int_fast at the bottom is
 * faster than newton_all_slow above; but newton_int_fast sometimes fails to
 * find some roots no matter how high the specified precision is. I'll see if
 * I can somehow fix that. *)

(* Set of intervals (x,y) *)
type interval = (float * float) list

exception EmptyInterval

(* Sorts i and fixes any empty or overlapping sub-intervals *)
let fix (i : interval) : interval =
  let i = List.sort ~cmp:(fun (x,_) (y,_) -> Float.compare x y) i in
  let rec fix_rec (a : interval) : interval =
    match a with
    | [] | [_] -> a
    | (x1,x2) :: (y1,y2) :: a' ->
      if x2 >= y1 then fix_rec ((x1,Float.max x2 y2) :: a')
      else if x1 = x2 then fix_rec ((y1,y2) :: a')
      else (x1,x2) :: fix_rec ((y1,y2) :: a')
  in fix_rec i

(* Function for testing *)
let rec rand_interval (len : int) (min : float) (max : float) : interval =
  if len = 0 then [] else
  let x = min +. Random.float (max -. min) in
  let y = x +. Random.float ((max -. x) /. 5.) in
  (x,y) :: (rand_interval (len - 1) min max)

let union (i1 : interval) (i2 : interval) : interval = fix (i1 @ i2)

let rec is_in (f : float) (i : interval) : bool =
  let f = fun c (x,y) -> c || (f >= x && f <= y) in
  List.fold_left ~f ~init:false i

let rec is_in_interior (f : float) (i : interval) : bool =
  let f = fun c (x,y) -> c || (f > x && f < y) in
  List.fold_left ~f ~init:false i

(* Returns the lowest and highest elements in an interval. *)
let rec bounds (i : interval) : float * float =
  match i with
  | [] -> raise EmptyInterval
  | [x] -> x
  | (l,_) :: i' -> let (_,h) = bounds i' in (l, h)

let subtract (i1 : interval) (i2 : interval) : interval =
  let rec sub_helper (i : interval) ((x,y) : float * float) : interval =
    match i with
    | [] -> []
    | (x',y') :: i' ->
      if x >= x' && x < y' then
	if y < y' then (x',x) :: (y,y') :: i'
	else if y = y' then (x',x) :: i'
	else (x',x) :: (sub_helper i' (x,y))
      else if y > x' && y <= y' then (y,y') :: i'
      else if x < x' && y > y' then sub_helper i' (x,y)
      else (x',y') :: sub_helper i' (x,y)
  in fix (List.fold_left ~f:(fun i j -> sub_helper i j) ~init:i1 i2)

(* Solves the equation p=0 on the interval i, and returns a list of roots, found
 * using Newton's method with epsilon = answer_prec. Increments each successive
 * try by try_prec. *)
(* Note: This version often gives an incomplete list of roots. *)
let newton_all_fast (p : poly) (i : interval) (try_prec : float) (duplicate_prec : float) (answer_prec : float) : float list =
  let rec newton_int_rec (i : interval) (curr : float list) : float list =
    let g : float option =
      try (let (lower,upper) = bounds i in
      let rec next_guess_rec (l : float) : float option =
	if l > upper then None
	else if is_in_interior l i || l = lower || l = upper then Some l
	else next_guess_rec (l +. try_prec)
      in next_guess_rec lower)
      with EmptyInterval -> None
    in
    match g with
    | None -> curr
    | Some f ->
      let root : float = newton p f answer_prec in
      let new_int = subtract i [(Float.min f root, Float.max f root +. try_prec)] in
      Printf.printf "New interval:";
      List.iter new_int ~f:(fun (x,y) -> Printf.printf " (%f,%f)" x y);
      Printf.printf "\n";
      let new_list =
        let f = fun b r -> b || Float.abs (r -. root) < duplicate_prec in
        if List.fold_left ~f ~init:false curr then curr
        else let _ = Printf.printf "New root: %f\n" root in root :: curr in
      newton_int_rec new_int new_list
  in newton_int_rec i []
