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

let evaluate (p : poly) (v : float) : float =
  let vals : float array = Array.mapi ~f:(fun i x -> x *. (v ** float i)) p in
  Array.fold_right ~f:(+.) ~init:0. vals

let derivative (p : poly) : poly =
  let p' : poly = Array.mapi ~f:(fun i x -> float i *. x) p in
  let len : int = Array.length p' - 1 in
  let ans : poly = Array.create ~len 0. in
  Array.blit ~src:p' ~src_pos:1 ~dst:ans ~dst_pos:0 ~len;
  ans

let rec newton (p : poly) (g : float) (epsilon : float) : float option =
  let abs_float : float -> float = fun x -> if x > 0. then x else (-.x) in
  if abs_float (evaluate p g) < epsilon then Some g
  else
    let deriv = evaluate (derivative p) g in
    if deriv = 0. then None
    else
      let g' = g -. (evaluate p g) /. deriv in
      newton p g' epsilon

(* Same as function above, but assumes known derivative d *)
let rec newton_d (p : poly) (d : poly) (g : float) (epsilon : float) : float option =
  let abs_float : float -> float = fun x -> if x > 0. then x else (-.x) in
  if abs_float (evaluate p g) < epsilon then Some g
  else
    let deriv = evaluate d g in
    if deriv = 0. then None
    else 
      let g' = g -. (evaluate p g) /. deriv in
      newton_d p d g' epsilon

(* Fairly slow method of repeatedly applying Newton's method to find all roots
 * of p, between lower and upper. Increments each try by try_prec, and uses
 * answer_prec = epsilon. Ignores duplicates that are within duplicate_prec of
 * each other. *)
(* Note: This function is much more naive and slower than newton_all_fast below,
 * but if the precision values and lower/upper are right, newton_all_slow should
 * never fail to find all real roots between lower and upper, while the faster
 * function newton_all_fast might. *)
let rec newton_all_slow (p : poly) ((lower, upper) : float * float) (try_prec : float) (duplicate_prec : float) (answer_prec : float) : float list =
  let rec newton_int_rec (d : poly) ((low, high) : float * float) (curr : float list) : float list =
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
  | 1 -> Printf.printf "Found %i solutions, expected %i. Solutions:" num_solutions degree;
	 List.iter ~f:(fun x -> Printf.printf " %f" x) solutions; Printf.printf "\n"; flush_all ();
	 let duplicate_prec = duplicate_prec /. 2. in
	 let answer_prec = answer_prec /. 2. in
	 Printf.printf "Using try precision %f, duplicate precision %f, and answer precision %f.\n" try_prec duplicate_prec answer_prec;
	 flush_all ();
	 newton_all_slow p (lower,upper) try_prec duplicate_prec answer_prec
  | 0 -> Printf.printf "Found correct number of solutions.\n"; flush_all (); solutions
  | _ -> Printf.printf "Found %i solutions, expected %i.\n" num_solutions degree;
	 Printf.printf "Solutions:"; List.iter ~f:(fun x -> Printf.printf " %f" x) solutions; Printf.printf "\n";
	 let try_prec = try_prec *. 2. in
	 let duplicate_prec = duplicate_prec *. 2. in
	 let answer_prec = answer_prec /. 10. in
	 newton_all_slow p (lower,upper) try_prec duplicate_prec answer_prec


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

let is_in (f : float) (i : interval) : bool =
  let f = fun c (x,y) -> c || (f >= x && f <= y) in
  List.fold_left ~f ~init:false i

let is_in_interior (f : float) (i : interval) : bool =
  let f = fun c (x,y) -> c || (f > x && f < y) in
  List.fold_left ~f ~init:false i

(* Returns the lowest and highest elements in an interval. *)
let rec bounds (i : interval) : float * float =
  match i with
  | [] -> raise EmptyInterval
  | [x] -> x
  | (l,_) :: i' -> let (_,h) = bounds i' in (l, h)

(* Subtracts i2 from i1.*)
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
      match newton p f answer_prec with
      | None ->
	let new_int = subtract i [(f, f +. try_prec)] in
	newton_int_rec new_int curr
      | Some root ->
        let new_int = subtract i [(Float.min f root, Float.max f root +. try_prec)] in
	let new_list =
          let f = fun b r -> b || Float.abs (r -. root) < duplicate_prec in
          if List.fold_left ~f ~init:false curr then curr else root :: curr in
	newton_int_rec new_int new_list
  in newton_int_rec i []
