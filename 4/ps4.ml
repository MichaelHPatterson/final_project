(* PS4
 * CS51 Spring 2014
 * Authors: Madhu Vijay & Michael Patterson
 * Exercises related to modules and functors.
 *)

open Core.Std

exception ImplementMe

type order = Equal | Less | Greater

(* Initializes pseudo-random number generator, used for testing sorts. *)
let _ = Random.self_init ()

(*****************************************************************************)
(*                               Part 2                                      *)
(*****************************************************************************)

(* A better signature for a binary tree, avoiding the comparison function
 * found in motivation.ml. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt

  (* What this type actually looks like is left up to the
   * particular BINTREE implementation (i.e. the struct) *)
  type tree

  (* Returns an empty tree *)
  val empty : tree

  (* Search a binary tree for the given value. *)
  val search : elt -> tree -> bool

  (* Insert elt into tree *)
  val insert : elt -> tree -> tree

  (* Delete the given value from a binary tree.
   * May raise NodeNotFound exception. *)
  val delete : elt -> tree -> tree

  (* Return the minimum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmin : tree -> elt

  (* Return the maximum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmax : tree -> elt

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* A signature for a module which defines a type and
 * how to compare values of that type, as well as ways of generating
 * values of that type. *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* Generate a value of type t *)
  val generate: unit -> t

  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t

  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t

  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  val generate_between: t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module IntCompare : COMPARABLE with type t=int =
struct
  type t = int

  let compare x y = if x < y then Less else if x > y then Greater else Equal

  let to_string = string_of_int

  let generate () = 0

  let generate_gt x () = x + 1

  let generate_lt x () = x - 1

  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* Another example implementation that only uses the first part
 * of the tuple in comparisons. *)
module IntStringCompare : COMPARABLE with type t=(int * string) =
struct
  type t = int * string
  let compare (p1,_) (p2,_) =
    if p1 < p2 then Less else if p1 > p2 then Greater else Equal

  let to_string (p, s) = "(" ^ string_of_int p ^ "," ^ s ^ ")"


  let () = Random.self_init ()

  let generate () = (0, string_of_int (Random.int Int.max_value))

  let generate_gt (p,s) () = (p+1, s)

  let generate_lt (p,s) () = (p-1, s)

  let generate_between (p1,_) (p2,s2) () =
    let (lower, higher) = (min p1 p2, max p1 p2) in
    (* Reuse the string from the second argument in the output value *)
    if higher - lower < 2 then None else Some (higher - 1, s2)
end


(* Functor that takes a COMPARABLE module and produces a binary search
 * tree matching the BINTREE signature. *)
module BinSTree(C : COMPARABLE) : BINTREE with type elt = C.t =
struct
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in
   * this is the only place you explicitly need to use C.t; you
   * should use elt everywhere else *)
  type elt = C.t

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt list * tree

  (* Representation of the empty tree *)
  let empty = Leaf


(*>* Problem 2.0 *>*)

  (* Inserts an element x into the tree t in the correct place. Puts x
   * at the front of a list, if a list of equal values exists. *)
  let rec insert (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> Branch (Leaf, [x], Leaf)
    | Branch (l, lst, r) ->
      match lst with
      (* This "failwith" matches the code provided for "delete". *)
      | [] -> failwith "Invalid tree: empty list as node"
      | hd :: _ -> 
        match C.compare x hd with
	| Less -> Branch (insert x l, lst, r)
	| Greater -> Branch (l, lst, insert x r)
	| Equal -> Branch (l, x :: lst, r)

(*>* Problem 2.1 *>*)

  (* Returns true iff the element x is in tree t. Uses C.to_string
   * as a stronger criterion than C.compare returning "Equal". *)
  let rec search (x : elt) (t : tree) : bool =
    match t with
    | Leaf -> false
    | Branch (l, lst, r) ->
      match lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd :: _ ->
        match C.compare x hd with
	| Less -> search x l
	| Greater -> search x r
	| Equal -> 
	  List.fold_right ~f:(fun x' -> (||) (C.to_string x' = C.to_string x))
                          ~init:false lst

  (* Removes the node with the minimum value, returning that node and the new
   * tree without that node. *)
  let rec pull_min (t : tree) : elt list * tree =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))


  (* Removes an element from the tree. If multiple elements are in the list,
   * removes the one that was inserted first.  *)
  let rec delete (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> raise NodeNotFound
    | Branch (l, lst, r) ->
      (* Reverse the list so that we pop off the last element in the list *)
      match List.rev lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl ->
        match C.compare x hd with
        | Less -> Branch (delete x l, lst, r)
        | Greater -> Branch (l, lst, delete x r)
        | Equal ->
          match tl with
          | _::_ -> Branch (l, List.rev tl, r)
          (* The list in the node is empty, so we have to
           * remove the node from the tree.  *)
          | [] ->
            match l, r with
            | Leaf, _ -> r
            | _, Leaf -> l
            | _ -> let v, r' = pull_min r in Branch (l,v,r')

(*>* Problem 2.2 *>*)

  (* Returns the minimum value of the tree t. Returns the minimum value that
   * was inserted first, if there's a tie. *)
  let getmin (t : tree) : elt =
    let rec min_helper = function
      | Leaf -> raise EmptyTree
      | Branch (Leaf, lst, _) ->
	(match List.rev lst with
	| [] -> failwith "Invalid tree: empty list as node"
	| x :: _ -> x)
      | Branch (l, _, _) -> min_helper l in
    min_helper t

(*>* Problem 2.3 *>*)

  (* Returns the maximum value of the tree t. *)
  let getmax (t : tree) : elt =
    let rec max_helper = function
      | Leaf -> raise EmptyTree
      | Branch (_, lst, Leaf) -> (
	match List.rev lst with
	| [] -> failwith "Invalid tree: empty list as node"
	| x :: _ -> x)
      | Branch (_, _, r) -> max_helper r in
    max_helper t

  (* Tests the insert function. *)
  let test_insert () =
    let x = C.generate () in
    let t = insert x empty in
    assert (t = Branch(Leaf, [x], Leaf));
    let t = insert x t in
    assert (t = Branch(Leaf, [x;x], Leaf));
    let y = C.generate_gt x () in
    let t = insert y t in
    assert (t = Branch(Leaf, [x;x], Branch(Leaf, [y], Leaf)));
    let z = C.generate_lt x () in
    let t = insert z t in
    assert (t = Branch(
                        Branch(Leaf, [z], Leaf),
                        [x;x],
                        Branch(Leaf, [y], Leaf)
                      ));
    ()

  (* Insert a bunch of elements, and test to make sure that we
   * can search for all of them. *)
  let test_search () =
    let x = C.generate () in
    let t = insert x empty in
    assert (search x t);
    let order = [ true; false; true; true; true; false; false] in
    let full_tree, values_inserted =
      List.fold_right
        ~f:(fun current_order (tree_so_far, values_so_far) ->
          let prev_value =
            match values_so_far with
            | [] -> x
            | hd :: _ -> hd
          in
          let value =
            if current_order
            then C.generate_gt prev_value ()
            else C.generate_lt prev_value ()
          in
          insert value tree_so_far, value :: values_so_far
        ) ~init:(t, []) order
    in
    List.iter ~f:(fun value -> assert (search value full_tree)) values_inserted

  (* Test function for getmax. *)
  let test_getmax () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (getmax (insert x4 (insert x3 (insert x2 (insert x empty)))) = x)

  (* Test function for getmin. *)
  let test_getmin () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (getmin (insert x2 (insert x4 (insert x (insert x3 empty)))) = x)

  (* Test function for delete. *)
  let test_delete () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      let after_ins = insert x4 (insert x3 (insert x2 (insert x empty))) in
      assert (delete x (delete x4 (delete x3 (delete x2 after_ins))) = empty)

  (* Runs all the test functions. *)
  let run_tests () =
    test_insert ();
    test_search ();
    test_getmax ();
    test_getmin ();
    test_delete ();
    ()

end

(* Defines an int binary tree using the BinSTree functor and the IntCompare
 * module. Used for testing below. *)
module IntTree = BinSTree(IntCompare)

(* Runs the tests from BinSTree. *)
let _ = IntTree.run_tests ()



(*****************************************************************************)
(*                               Part 3                                      *)
(*****************************************************************************)

(* Signature for a priority queue *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(*>* Problem 3.0 *>*)

(* Priority queue implementation using lists. *)
module ListQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct
  exception QueueEmpty

  (* Element type *)
  type elt = C.t

  (* Queue type *)
  type queue = elt list

(*>* Problem 3.1 *>*)
  (* The empty list *)
  let empty = []

(*>* Problem 3.2 *>*)
  (* Checks if t is the empty list *)
  let is_empty (t : queue) = (t = empty)


(*>* Problem 3.3 *>*)

  (* Adds an element to a queue, in the correct order. Adds newer elements
   * later if there is a tie. *)
  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | e' :: q' ->
      match C.compare e e' with
      | Less -> e :: q
      | Equal | Greater -> e' :: add e q'

(*>* Problem 3.4 *>*)

  (* Returns the first (i.e. highest-priority) element and the rest of the
   * queue. Raises QueueEmpty exception if necessary. *)
  let take (q : queue) =
    match q with
    | [] -> raise QueueEmpty
    | e :: q' -> (e, q')

  (* Simple testing function for "empty" and "is_empty". *)
  let test_empty () =
    assert(is_empty empty);
    let x = C.generate () in assert(not (is_empty (add x empty)))

  (* Helper function for test_add and test_take. Generates a queue of <size>
   * elts, all of which exceed <last>. *)
  let rec gen_queue (size : int) (last : elt) : queue =
    if size = 0 then empty
    else
      let next = C.generate_gt last () in
      add next (gen_queue (size - 1) next)

  (* Tests "add" by creating a queue of 1000 elts and checking that it is
   * in the right order. *)
  let test_add () =
    let rec check_queue (q : queue) : bool =
      match q with
      | [] | [_] -> true
      | e1 :: e2 :: q' ->
        (C.compare e1 e2 <> Greater && check_queue (e2 :: q'))
    in
    assert(check_queue (gen_queue 1000 (C.generate ())))

  (* Test "take" by creating a queue of 1000 elts, removing the elements,
   * and making sure they come out in the right order. *)
  let test_take () = 
    let rec check_priority (e : elt) (q : queue) : bool =
      match q with
      | [] -> true
      | e' :: q' -> (C.compare e e' <> Greater && check_priority e' q')
    in
    let min_val = C.generate () in
    assert(check_priority min_val (gen_queue 1000 min_val))

  (* Runs test functions for the ListQueue. *)
  let run_tests () =
    test_empty ();
    test_add ();
    test_take ();
    ()
end

(* List priority queue of ints, for testing *)
(* Added ' to differentiate from IntListQueue used for sorting, to prevent
 * compiler error. *)
module IntListQueue' = ListQueue(IntCompare)

(* Runs test functions for ListQueue. *)
let _ = IntListQueue'.run_tests ()



(*>* Problem 3.5 *>*)

(* Priority queue implementation using a binary search tree. *)
module TreeQueue(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
struct
  exception QueueEmpty

  (* Binary search tree module using the COMPARABLE module passed in *)
  module T = (BinSTree(C) : BINTREE with type elt = C.t)

  (* Element type *)
  type elt = T.elt
  
  (* Queue type *)
  type queue = T.tree

  (* Empty tree, from the BinSTree module. *)
  let empty : queue = T.empty

  (* Checks if q is the empty tree. *)
  let is_empty (q : queue) : bool = (q = empty)

  (* Uses the "insert" function from BinSTree to add an element. *)
  let add (e : elt) (q : queue) : queue = T.insert e q

  (* Uses "getmin" to take the highest-priority element and return
   * the rest of the tree.
   * Throws "QueueEmpty" instead of "EmptyTree" to maintain abstraction. *)
  let take (q : queue) : elt * queue =
    try (let e : elt = T.getmin q in (e, T.delete e q))
    with T.EmptyTree -> raise QueueEmpty

  (* Short testing function for "empty" and "is_empty". *)
  let test_empty () =
    assert (is_empty T.empty);
    assert (not (is_empty (add (C.generate ()) empty)))

  (* Tests "add" by adding 1000 elements to a queue, and checking each time to
   * make sure the last element was properly added. *)
  let test_add () =
    let rec test_add_helper (size : int) (last : elt) (curr : queue) : unit =
      assert(T.search last curr);
      if size = 0 then ()
      else
        let next = C.generate_gt last () in
        test_add_helper (size - 1) next (add next curr)
    in
    let first = C.generate () in
    test_add_helper 1000 first (add first empty)

  (* Tests "take" (and, indirectly, "add"). Creates a queue of 1000 elements,
   * removes those elements, and makes sure they come out in order. *)
  let test_take () =
    let rec gen_queue (size : int) (last : elt) : queue =
      if size = 0 then empty
      else
        let next = C.generate_gt last () in
        add next (gen_queue (size - 1) next)
    in
    let rec check_priority (e : elt) (q : queue) : bool =
      if is_empty q then true
      else
        let (e', q') = take q in
        (C.compare e e' <> Greater && check_priority e' q')
    in
    let min_val = C.generate () in
    assert(check_priority min_val (gen_queue 1000 min_val))

  (* Runs the test functions for TreeQueue. *)
  let run_tests () =
    test_empty ();
    test_add ();
    test_take ();
    ()

end

(* TreeQueue of ints using IntCompare, for testing. *)
(* Added ' to differentiate from IntTreeQueue used for sorting, to prevent
 * compiler error. *)
module IntTreeQueue' = TreeQueue(IntCompare)

(* Run tests on IntTreeQueue. *)
let _ = IntTreeQueue'.run_tests ()



(*****************************************************************************)
(*                               Part 4                                      *)
(*****************************************************************************)

(*>* Problem 4.0 *>*)

(* Implementation of a priority queue using a binary heap. *)
module BinaryHeap(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty

  type elt = C.t

  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* A tree is either just a single element, has one branch (the first elt in
   * the tuple is the element at this node, and the second elt is the element
   * down the branch), or has two branches (with the node being even or odd) *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  (* An empty queue *)
  let empty = Empty

  (* Checks whether a queue is empty *)
  let is_empty (q : queue) = q = Empty

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    (* Given a tree, where e will be inserted is deterministic based on the
     * invariants. If we encounter a node in the tree where its value is
     * greater than the element being inserted, then we place the new elt in
     * that spot and propagate what used to be at that spot down toward where
     * the new element would have been inserted *)
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* Simply returns the top element of the tree t. *)
  let get_top (t : tree) : elt =
    match t with
    | Leaf e
    | OneBranch (e, _)
    | TwoBranch (_, e, _, _) -> e

  (* Takes a tree, and if the top node is greater than its children, fixes
   * it. If fixing it results in a subtree where the node is greater than its
   * children, then it recursively fixes the subtree tree. Assumes that
   * everything is in order at the start, except the head element. *)
  let rec fix (t : tree) : tree =
    match t with
    | Leaf _ -> t
    | OneBranch (e1, e2) ->
      if (C.compare e1 e2 = Less) then t
      else OneBranch (e2, e1)
    | TwoBranch (b, e, t1, t2) ->
      let switch_left ((b, e, t1, t2) : balance * elt * tree * tree) : tree =
         (match t1 with
	 | Leaf e' -> TwoBranch (b, e', Leaf e, t2)
	 | OneBranch (e1, e2) -> TwoBranch(b, e1, fix (OneBranch (e, e2)), t2)
         | TwoBranch (b', e', t1', t2') ->
           TwoBranch (b, e', fix (TwoBranch (b', e, t1', t2')), t2))
      in
      let switch_right ((b, e, t1, t2) : balance * elt * tree * tree) : tree =
         (match t2 with
	 | Leaf e' -> TwoBranch (b, e', t1, Leaf e)
	 | OneBranch (e1, e2) -> TwoBranch(b, e1, t1, fix (OneBranch (e, e2)))
         | TwoBranch (b', e', t1', t2') ->
           TwoBranch (b, e', t1, fix (TwoBranch (b', e, t1', t2'))))
      in
      let top1 : elt = get_top t1 in
      let top2 : elt = get_top t2 in
      match C.compare e top1, C.compare e top2 with
      | Greater, Greater ->
        if (C.compare top1 top2 = Less) then fix (switch_left (b, e, t1, t2))
	else fix (switch_right (b, e, t1, t2))
      | Greater, _ -> switch_left (b, e, t1, t2)
      | _, Greater -> switch_right (b, e, t1, t2)
      | _ -> t
  
  (* Takes a queue and returns the tree it contains (or QueueEmpty if
   * it doesn't contain a tree. *)
  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t -> t

  (* Takes a queue, and returns (1) the item that was most recently added to
   * its bottom-level, and (2) the queue that results from removing that
   * element. *)
  let rec get_last (t : tree) : elt * queue =
    match t with
    | Leaf e -> (e, Empty)
    | OneBranch (e1, e2) -> (e2, Tree (Leaf e1))
    | TwoBranch (Even, e, t1, t2) ->
      (let (e', q') = get_last t2 in
      match q' with
      | Empty -> (e', Tree (OneBranch (e, get_top t1)))
      | Tree t2' -> (e', Tree (TwoBranch (Odd, e, t1, t2'))))
    | TwoBranch (Odd, e, t1, t2) ->
      (let (e', q') = get_last t1 in
      match q' with
      | Empty -> (e', Tree (OneBranch (e, get_top t2)))
      | Tree t1' -> (e', Tree (TwoBranch (Even, e, t1', t2))))

  (* Removes the top elt in the tree, and returns that elt and a new tree. *)
  let take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    (* Implement the odd case! *)
    | TwoBranch (Odd, e, t1, t2) ->
      let (last, q1') = get_last t1 in
      (* No need for matching -- the only possible previous state is an Odd
       * TwoBranch Tree. *)
      (e, Tree (fix (TwoBranch (Even, last, extract_tree q1', t2))))


  (* Tests for take. Elts are added, and 'take' is tested on resulting heaps.
   * Note the order of priority for elts: c < w < x < y < z < a < b < d *)
  let run_tests () =
    (* Add elt to make one-elt heap, and test 'take' *)
    let x = C.generate () in
    let t = add x empty in
    assert (t = Tree (Leaf x));
    assert (take t = (x, Empty));

    (* Add elt to make two-elt heap, and test 'take' *)
    let t = add x t in
    assert (t = Tree( OneBranch (x,x)));
    assert (take t = (x, Tree(Leaf x)));

    (* Add elt to make three-elt heap, and test 'take' *)
    let y = C.generate_gt x () in
    let t = add y t in
    assert (t = Tree( TwoBranch (Even, x, Leaf x, Leaf y)));
    assert (take t = (x, Tree (OneBranch (x,y))));

    (* Add elts to make seven-elt heap, and test 'take' *)
    let z = C.generate_gt y () in
    let t = add z t in
    assert (t = Tree (TwoBranch (Odd, x, OneBranch (x,z), Leaf y)));
    let a = C.generate_gt z () in
    let t = add a t in
    assert (t = Tree (TwoBranch (Even, x, OneBranch (x,z), OneBranch(y,a))));
    let b = C.generate_gt a () in
    let t = add b t in
    assert (t = Tree (TwoBranch (Odd, x, TwoBranch (Even, x, Leaf z, Leaf b),
            OneBranch(y,a))));
    let c = C.generate_lt x () in
    let t = add c t in
    assert (t = Tree (TwoBranch (Even, c, TwoBranch (Even, x, Leaf z, Leaf b),
            TwoBranch (Even, x, Leaf a, Leaf y))));
    assert(take t = (c, Tree (TwoBranch (Odd, x, 
           TwoBranch (Even, x, Leaf z, Leaf b), OneBranch(y,a)))));

    (* Test 'take' after another the first 'take' on seven-elt *)
    let (test, t') = take (snd (take t)) in
    assert ((test, t') = (x, Tree (TwoBranch (Even, x, OneBranch (z, b),
            OneBranch (y, a)))));

    (* Add elt to make eight-elt heap, and test 'take' *)
    let d = C.generate_gt b () in
    let t = add d t in
    assert (t = Tree (TwoBranch (Odd, c, TwoBranch(Odd, x, (OneBranch (z,d)),
            Leaf b), TwoBranch (Even, x, Leaf a, Leaf y))));
    assert (take t = (c, Tree (TwoBranch (Even, x, TwoBranch(Even, x, Leaf z,
            Leaf b), TwoBranch (Even, y, Leaf a, Leaf d)))))    
end

(* Defining an IntHeapQueue with the BinaryHeap functor *)
(* Added ' to differentiate from IntHeapQueue used for sorting, to prevent
 * compiler error. *)
module IntHeapQueue' = (BinaryHeap(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* Using testing functions in the BinaryHeap *)
let _ = IntHeapQueue'.run_tests ()



(* Priority queue modules for use in sorting.
 *: Added ' after the module names, to make the names different from
 * their earlier equivalents, to prevent compiler errors. *)
module IntListQueue = (ListQueue(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)
module IntHeapQueue = (BinaryHeap(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* store the whole modules in these variables *)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = IntCompare.t)
let tree_module = (module IntTreeQueue : PRIOQUEUE with type elt = IntCompare.t)

(* Implements sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in

  let rec extractor pq lst =
    if P.is_empty pq then lst else
    let (x, pq') = P.take pq in
    extractor pq' (x::lst) in
  let pq = List.fold_right ~f:P.add ~init:P.empty lst in
  List.rev (extractor pq [])


(* Sort function with a priority queue using a heap implementation. *)
let heapsort = sort heap_module

(* Sorting with a priority queue with a tree implementation. *)
let treesort = sort tree_module

(* This is really insertion sort; sorts with a priority queue using ordered
 * lists. The name was preserved so as not to confuse you. *)
let selectionsort = sort list_module

(* Function that sorts a test function using large int lists of size <size>. *)
let sort_tester (sort : int list -> int list) (size : int) : unit =
  let rec rand_list (size : int) : int list =
    if size = 0 then []
    else
      let value : int = Random.int Int.max_value - (Int.max_value / 2) in
      value :: (rand_list (size - 1))
  in
  let rec check_non_decr : int list -> bool = function
    | [] | [_] -> true
    | x :: y :: lst' -> x <= y && check_non_decr (y :: lst')
  in assert (check_non_decr (sort (rand_list size)))

(* Use sort_tester to test the three sort functions. *)
let _ = sort_tester heapsort 10000
let _ = sort_tester treesort 10000
let _ = sort_tester selectionsort 1000



(*****************************************************************************)
(*                               Part N                                      *)
(*****************************************************************************)

(*>* Problem N.0 *>*)

(* Functor that takes a COMPARABLE module as an argument and enables sorting
 * on the type defined by that module, using BinaryHeap and the "sort"
 * function inside. *)
module Sorter (C : COMPARABLE) =
struct
  (* Element type derived from the COMPARABLE module passed in. *)
  type c = C.t

  (* Heap created using BinaryHeap *)
  module CHeap = BinaryHeap(C)

  (* Sorts a list of c's by turning them into a heap and then extracting
   * them from the heap in order. *)
  let sort (lst : c list) : c list =
    let newheap = List.fold_right ~f:CHeap.add ~init:CHeap.empty lst in
    let rec extract (heap : CHeap.queue) (curr_list : c list) =
      if CHeap.is_empty heap then curr_list
      else
        let (x, heap') = CHeap.take heap in
        extract heap' (x :: curr_list)
    in
    List.rev (extract newheap [])
end


(*>* Problem N.1 *>*)

(*>* Problem N.2 *>*)

(* Minutes spent by both of us, combined *)
let minutes_spent : int = 1000
