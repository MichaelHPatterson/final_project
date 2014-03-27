(* Partner 1's name: Madhusudan ("Madhu") Vijay
 * Partner 1's code.seas account: mvijay
 *
 * Partner 2's name: Michael Patterson
 * Partner 2's code.seas account: michaelpatterson
 *
 * CS51, Problem Set 6
 * March 28, 2014
 *
 * Part 2: Trees and Streams
 *)

open Core.Std

(* Definition for infinite binary treestreams, analogous to the definition of
 * streams provided in lecture. *)
type 'a tree = unit -> 'a tr
and 'a tr = Stem of 'a * 'a tree * 'a tree ;;


(*>* Problem 2.1.a *>*)
(* Takes an 'a treestream and returns the value at the root of the tree *)
let headt (t: 'a tree) : 'a = let Stem (x, _, _) = t () in x
;;


(*>* Problem 2.1.b *>*)
(* ltail and rtail take a treestream and return its left and right subtrees,
 * respectively. *)
let ltail (t: 'a tree) : 'a tree = let Stem (_, y, _) = t () in y
;;

let rtail (t: 'a tree) : 'a tree = let Stem (_, _, z) = t () in z
;;


(*>* Problem 2.1.c *>*)
(* Takes a function f and maps it over the given treestream *)
let rec mapt (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  fun () -> Stem (f (headt t), mapt f (ltail t), mapt f (rtail t))
;;


(*>* Problem 2.1.d *>*)
(* Takes a function f and two treestreams t1 and t2 and combines them into
 * one treestream. If x1 and x2 are the values at corresponding locations
 * in t1 and t2 respectively, then the corresponding value in "zipt f t1 t2"
 * would be "f x1 x2" *)
let rec zipt (f: 'a -> 'b -> 'c) (t1: 'a tree) (t2: 'b tree) : 'c tree =
  fun () -> Stem (f (headt t1) (headt t2), zipt f (ltail t1) (ltail t2),
	          zipt f (rtail t1) (rtail t2))
;;


(*>* Problem 2.1.e *>*)
(* Defines a treestream of all ones by placing 1 at the head and recursively
 * calling itself. *)
let rec onest () = Stem (1, (fun () -> onest ()), (fun () -> onest ()))
;;


(*>* Problem 2.1.f *>*)

(* Defines a treestream in which each positive natural number appears
 * exactly once, and where the first few rows are 1; 2 3; 4 5 6 7,
 * etc. Uses helper function that returns tr with head n, head of left tree =
 * n * 2, and head of right tree = (n * 2) + 1 *)
let treenats () =
  let rec nathelp (n : int) : int tr =
    Stem (n, (fun () -> nathelp (n * 2)), (fun () -> nathelp ((n * 2) + 1))) in
  nathelp 1
;;


(***************** Using the Lazy module ******************)

open Lazy

type 'a stream = Cons of 'a * 'a stream Lazy.t

let rec ones = Cons(1, lazy(ones));;


(*>* Problem 2.2.a *>*)

(* Returns the head of s. *)
let head (s:'a stream) : 'a = let Cons (x, _) = s in x
;;

(* NOT required, but used for testing purposes and code below *)
let tail (s:'a stream) : 'a stream =
  let Cons(_, x) = s in Lazy.force x


(*>* Problem 2.2.b *>*)

(* Maps a function f over the stream s. *)
let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  let Cons (x, y) = s in
  Cons (f x, lazy (map f (Lazy.force y)))
;;


(*>* Problem 2.2.c *>*)

(* A stream of the natural numbers *)
let rec nats = Cons (1, lazy (map ((+) 1) nats)) ;;


(*>* Problem 2.2.d *>*)

(* Returns the nth element of a stream. The function is zero-indexed (i.e.
 * "nth 0 s" is the same as "head s". *)
let rec nth (n:int) (s:'a stream) : 'a =
  if n <= 0 then head s
  else nth (n - 1) (tail s)
;;


(*>* Problem 2.2.e *>*)

(* Takes two int streams (s1 and s2) sorted in ascending order, and merges
 * them into a single sorted stream s, while removing duplicates. *)
let rec merge (s1:int stream) (s2:int stream) : int stream =
  (* advances the stream to the next value that is <> current value being added.
   * in cases w/streams of infinite equivalence, this could infinitely loop. *)
  let rec next (n:int) (stream:int stream) : int stream =
    if head stream = n then next n (tail stream) else stream in
  if head s1 < head s2 then 
    let add = head s1 in
    Cons (add, lazy (merge (next add (tail s1)) (next add (tail s2))))
  else
    let add = head s2 in
    Cons (add, lazy (merge (next add s1) (next add (tail s2))))
;;


(*>* Problem 2.2.f *>*)
(* What problems can we run into with this conception of "merge"? What
 * if we were to run "merge ones ones"? Answer within the comment. *)

(*
 *  Answer: Because we are trying to prevent duplicates, we could end up in an
 *  infinite loop searching for the next element of a stream that is not a
 *  duplicate. In the case of "merge ones ones", we could find the first 1 of
 *  one stream and then potentially enter an infinite loop looking for the next 
 *  elt of either stream that is not 1. More specifically, we can find the head
 *  of "merge ones ones" (which is 1), but if we try to find the head of the 
 *  tail, we enter an infinite loop by trying to force finding  the next non-1
 *  element. *)


(*>* Problem 2.2.g *>*)

(* Takes an integer "n" and an int stream "s", and multiplies each element of
 * "s" by "n". *)
let scale n = map (fun x -> n * x) ;;


(*>* Problem 2.2.h *>*)

(* We didn't implement this *)
let selectivestream = failwith "Unimplemented" ;;


(*>* Problem 2.3 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 480
