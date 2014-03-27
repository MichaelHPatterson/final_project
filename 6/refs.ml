(* NAMES:
 *
 * Partner 1's name: Madhusudan ("Madhu") Vijay
 * Partner 1's code.seas account: mvijay
 *
 * Partner 2's name: Michael Patterson
 * Partner 2's code.seas account: michaelpatterson
 *
 * Problem set 6, part 1: functions related to mutable lists
 *)

open Core.Std

(* A type for mutable lists. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(* NOTE: ALL tests are at the bottom of the file, for logical coherence (since
 * all 3 functions are tested on each of the test lists) *)

(*>* Problem 1.1 *>*)
(* Returns true if the mutable list lst has a cycle. *)
let has_cycle (lst : 'a mlist) : bool =
  (* Checks the rest of the mutable list (rest) versus a list of previously
   * checked nodes (curr), for physical equality. *)
  let rec check (curr : 'a mlist list) (rest : 'a mlist) : bool =
    match rest with
    | Nil -> false
    | Cons(_, rest') ->
      let f = fun b mlst -> b || (phys_equal !rest' mlst) in
      let curr = rest :: curr in
      (List.fold_left ~f:f ~init:false curr) || (check curr !rest')
  in
  check [] lst

(*>* Problem 1.2 *>*)
(* Destructively flattens a mutable list (i.e. removes its cycle, if it
 * has any). *)
let flatten (lst : 'a mlist) : unit =
  let rec flatten_rec (curr : 'a mlist list) (rest : 'a mlist) : unit =
    match rest with
    | Nil -> ()
    | Cons(_, rest') ->
      (* Calling has_cycle here would look shorter but be less efficient 
       * because some operations would be repeated. *)
      let f = fun b x -> b || (phys_equal !rest' x) in
      let curr = rest :: curr in
      if List.fold_left ~f:f ~init:false curr then
        let _ = rest' := Nil in ()
      else flatten_rec curr !rest'
  in
  flatten_rec [] lst
;;

(*>* Problem 1.3 *>*)
(* Finds the number of nodes in a mutable list non-destructively. Ignores
 * cycles (i.e. returns the same length for either a cyclical list or its
 * flattened counterpart). *)
let mlength (lst : 'a mlist) : int =
  let rec mlength_rec (curr : 'a mlist list) (rest : 'a mlist) : int =
    match rest with
    | Nil -> 0
    | Cons(_, rest') ->
      (* Calling flatten here would be efficient but destructive. *)
      let f = fun b x -> b || (phys_equal !rest' x) in
      let curr = rest :: curr in
      (* If cycle found, then terminate and return 1 (i.e. count current
       * node but no more), else recur *)
      if List.fold_left ~f:f ~init:false curr then 1
      else 1 + mlength_rec curr !rest'
  in
  mlength_rec [] lst  
;;

(* Set of functions for testing *)

(* Non-cyclical mutable list for testing, from the distribution code. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

assert(not (has_cycle list1));
assert(mlength list1 = 3);
flatten list1;
(* Verifies that flatten leaves a non-cyclical list intact *)
assert(not (has_cycle list1));
assert(mlength list1 = 3);
assert(list1 = Cons(1, ref (Cons(2, ref (Cons(2, ref Nil))))));;

(* Cyclical list for testing, from the distribution code. *)
let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

assert(has_cycle list2);
assert(mlength list2 = 2);
flatten list2;
assert(not (has_cycle list2));
assert(mlength list2 = 2);
assert(list2 = Cons(1, ref (Cons(2, ref Nil))));;

(* Cyclical list identical to the example from the pset spec *)
let reflist3 = ref Nil
let list3 = Cons(2, ref (Cons(3, ref (Cons(2, reflist3)))))
let _ = reflist3 := list3
let list3 = Cons(1, ref list3)

assert(has_cycle list3);
assert(mlength list3 = 4);
flatten list3;
assert(not (has_cycle list3));
assert(mlength list3 = 4);
assert(list3 = Cons(1, ref (Cons(2, ref (Cons(3, ref (Cons(2, ref Nil))))))));;

(* Non-cyclical list identical to the example from the pset spec -- basically
 * a non-cyclical analog of list3 above *)
let list4a = Cons(2, ref Nil)
let list4b = Cons(3, ref list4a)
let list4c = Cons(2, ref list4b)
let list4 = Cons(1, ref list4c)

assert(not (has_cycle list4));
assert(mlength list4 = 4);
flatten list4;
assert(not (has_cycle list4));
assert(mlength list4 = 4);
assert(list4 = Cons(1, ref (Cons(2, ref (Cons(3, ref(Cons (2, ref Nil))))))));;

(* Empty list (corner case) *)
let list5 = Nil

assert(not (has_cycle list5));
assert(mlength list5 = 0);
flatten list5;
assert(not (has_cycle list5));
assert(mlength list5 = 0);
assert(list5 = Nil);;

(* 1-element non-cyclical list (sort of a corner case) *)
let list6 = Cons(10, ref Nil)

assert(not (has_cycle list6));
assert(mlength list6 = 1);
flatten list6;
assert(not (has_cycle list6));
assert(mlength list6 = 1);
assert(list6 = Cons(10, ref Nil));;

(* 1-element cyclical list (corner case) *)
let reflist7 = ref Nil
let list7 = Cons(7, reflist7)
let _ = reflist7 := list7

assert(has_cycle list7);
assert(mlength list7 = 1);
flatten list7;
assert(not (has_cycle list7));
assert(mlength list7 = 1);
assert(list7 = Cons(7, ref Nil));;


(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 200
