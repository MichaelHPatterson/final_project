(* NAMES:
 *
 * Partner 1's name: ______
 * Partner 1's code.seas account: _______
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: ______
 * Partner 2's code.seas account: _______
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  let rec check (curr : 'a mlist list) (rest : 'a mlist) : bool =
    match rest with
    | Nil -> false
    | Cons(_, rest') ->
      let f = (fun b mlst -> b || phys_equal (!rest') mlst) in
      (* This if-then-else is used instead of a simple "or" (i.e. ||), to
       * prevent an infinite loop that would arise for a cyclical list. *)
      if List.fold_left ~f:f ~init:false curr then true
      else check (rest :: curr) !rest'
  in
  check [] lst

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)
assert(not (has_cycle list1))

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2
assert(has_cycle list2)

(* Creates a cyclical list identical to the example from the pset spec *)
let reflist3 = ref Nil
let list3 = Cons(2, ref (Cons(3, ref (Cons(2, reflist3)))))
let _ = reflist3 := list3
let list3 = Cons(1, ref list3)
assert(has_cycle list3);;

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec flatten_rec (curr : 'a mlist list) (rest : 'a mlist) : unit =
    match rest with
    | Nil -> ()
    | Cons(_, rest') ->
      (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Some of this code is very redundant with the function above. However, calling the has_cycle function would lead to a lot of inefficiency/redundancy in execution *)
      let f = fun b x -> b || phys_equal (!rest') x in
      if List.fold_left ~f:f ~init:false curr then
        let _ = rest' := Nil in ()
      else flatten_rec (rest :: curr) !rest'
  in
  flatten_rec [] lst

(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  let rec mlength_rec (curr : 'a mlist list) (rest : 'a mlist) : int =
    match rest with
    | Nil -> 0
    | Cons(_, rest') ->
      (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Some of this code is very redundant with the code above. However, calling flatten would be destructive (i.e. it would change the mutable list), and calling has_cycle would lead to a lot of inefficiency/redundancy in execution *)
      let f = fun b x -> b || phys_equal (!rest') x in
      if List.fold_left ~f:f ~init:false curr then 1
      else 1 + mlength_rec (rest :: curr) !rest'
  in
  mlength_rec [] lst  

(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = -1
