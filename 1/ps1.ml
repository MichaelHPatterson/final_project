(*** CS 51 Problem Set 1 ***)
(*** January 31, 2014 ***)
(*** YOUR NAME HERE ***)

(* Open up the library we'll be using in this course *)
open Core.Std

(* Problem 1 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(*>* Problem 1a *>*)
(*
let prob1a : ??? = let greet y = "Hello " ^ y in greet "World!";;
*)

(*>* Problem 1b *>*)
(*
let prob1b : ??? = [Some 4; Some 2; None; Some 3];;
*)

(*>* Problem 1c *>*)
(*
let prob1c : ??? = ((None, Some 42.0), true);;
*)


(* Explain in a comment why the following will not type check,
   and provide a fix *)

(*>* Problem 1d *>*)
(*
let prob1d : string * int list = [("CS", 51); ("CS", 50)];;
*)

(*>* Problem 1e *>*)
(*
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4, 3.9) then 4 else 2;;
*)

(*>* Problem 1f *>*)
(*
let prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); ("April", None);
   ("May", None); ("June", 1); ("July", None); ("August", None);
   ("September", 3); ("October", 1); ("November", 2); ("December", 3)] ;;
*)



(* Problem 2 - Write the following functions *)
(* For each subproblem, you must implement a given function and corresponding
 * unit tests (i.e. assert expressions). You are provided a high level
 * description as well as a prototype of the function you must implement. *)

(*>* Problem 2a *>*)

(* `reversed lst` should return true if the integers in lst are in
 * decreasing order. The empty list is considered to be reversed. Consecutive
 * elements can be equal in a reversed list. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 2b for
 * examples of tests). *)


(*>* Problem 2b *>*)

(* merge takes two integer lists, each sorted in increasing order,
 and returns a single merged list in sorted order. For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

*)

(* The type signature for merge is as follows: *)
(* merge : int list -> int list -> int list *)

(*
(* sample tests *)
assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
assert ((merge [] []) = []);;
assert ((merge [1] []) = [1]);;
assert ((merge [] [-1]) = [-1]);;
assert ((merge [1] [-1]) = [-1;1]);;
*)


(*>* Problem 2c *>*)
(* unzip should be a function which, given a list of pairs, returns a
 * pair of lists, the first of which contains each first element of
 * each pair, and the second of which contains each second element.
 * The returned lists should have the elements in the order in which
 * they appeared in the input. So, for instance:

unzip [(1,2);(3,4);(5,6)];;
- : int list * int list = ([1;3;5],[2;4;6])

*)


(* The type signature for unzip is as follows: *)
(* unzip : (int * int) list -> int list * int list) *)


(*>* Problem 2d *>*)

(* `variance lst` returns None if lst has fewer than 2 floats, and
 * Some of the variance of the floats in lst otherwise.  Recall that
 * the variance of a sequence of numbers is 1/(n-1) * sum (x_i-m)^2,
 * where a^2 means a squared, and m is the arithmetic mean of the list
 * (sum of list / length of list). For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : int option = Some 2.5
variance [1.0];;
- : int option = None

 * Remember to use the floating point version of the arithmetic
 * operators when operating on floats (+. *., etc). The "float"
 * function can cast an int to a float. *)

(* variance : float list -> float option *)


(*>* Problem 2e *>*)

(* few_divisors n m should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. Note that this is *not* the
 * same as n having fewer divisors than m:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

 * Do not worry about negative integers at all. We will not test
 * your code using negative values for n and m, and do not
 * consider negative integers for divisors (e.g. don't worry about
 * -2 being a divisor of 4) *)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)


(*>* Problem 2f *>*)

(* `concat_list sep lst` returns one big string with all the string
 * elements of lst concatenated together, but separated by the string
 * sep. Here are some example tests:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

*)

(* The type signature for concat_list is: *)
(* concat_list : string -> string list -> string *)

(*>* Problem 2g *>*)

(* One way to compress a list of characters is to use run-length encoding.
 * The basic idea is that whenever we have repeated characters in a list
 * such as ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] we can
 * (sometimes) represent the same information more compactly as a list
 * of pairs like [(5,'a');(3,'b');(1,'c');(4,'d')].  Here, the numbers
 * represent how many times the character is repeated.  For example,
 * the first character in the string is 'a' and it is repeated 5 times,
 * followed by 3 occurrences of the character 'b', followed by one 'c',
 * and finally 4 copies of 'd'.
 *
 * Write a function to_run_length that converts a list of characters into
 * the run-length encoding, and then write a function from_run_length
 * that converts back. Writing both functions will make it easier to
 * test that you've gotten them right. *)

(* The type signatures for to_run_length and from_run_length are: *)
(* to_run_length : char list -> (int * char) list *)
(* from_run_length : (int * char) list -> char list *)

(*>* Problem 3 *>*)

(* Challenge!

 * permutations lst should return a list containing every
 * permutation of lst. For example, one correct answer to
 * permutations [1; 2; 3] is
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]].

 * It doesn't matter what order the permutations appear in the returned list.
 * Note that if the input list is of length n then the answer should be of
 * length n!.

 * Hint:
 * One way to do this is to write an auxiliary function,
 * interleave : int -> int list -> int list list,
 * that yields all interleavings of its first argument into its second:
 * interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
 * You may also find occasion for the library functions
 * List.map and List.concat. *)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)
