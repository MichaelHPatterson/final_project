open Core.Std

(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Equal -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right xs ~f:insert ~init:ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Equal -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left ~f:(fun a x -> f x a) ~init:e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left s ~f:f ~init:"") ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  (* generates a list of elts of size n all greater than each other, with
   * the first value being elt_init *)
  let rec greater_list (n : int) (elt_init : elt) : elt list =
    if n = 0 then [] else 
      let new_key = C.gen_gt elt_init () in 
      new_key :: (greater_list (n - 1) new_key)

  (* get the last elt in a list *)
  let rec last_elt (lst : elt list) : elt =
    match lst with
    | [] -> failwith "empty list - no last elt"
    | x :: [] -> x
    | _ :: xs -> last_elt xs
  
  (* checks if two sets have identical *)
  let rec set_compare (shifty_set : set) (good_set : set) : bool =
      match (choose shifty_set, choose good_set) with
      | (None, None) -> true
      | (None, Some _) -> false
      | (Some _, None) -> false
      | (Some (x, xs), _)  -> 
          if member good_set x then set_compare xs (remove x good_set)
	  else false
  
  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter elts ~f:(fun k -> assert(member s1 k)) ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right elts ~f:(fun k r -> remove k r) ~init:s1 in
    List.iter elts ~f:(fun k -> assert(not (member s2 k))) ;
    ()

  let test_union () = 
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let b_list = greater_list 100 (last_elt a_list) in
    let c_list = greater_list 100 (last_elt b_list) in
    let set1 = insert_list empty (a_list @ b_list) in
    let set2 = insert_list empty (b_list @ c_list) in
    let union_set = insert_list empty (a_list @ (b_list @ c_list)) in
    assert (set_compare (union set1 set2) union_set);
    ()

  let test_intersect () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let b_list = greater_list 100 (last_elt a_list) in
    let c_list = greater_list 100 (last_elt b_list) in
    let set1 = insert_list empty (a_list @ b_list) in
    let set2 = insert_list empty (b_list @ c_list) in
    let intersection_set = insert_list empty (b_list) in
    assert (set_compare (intersect set1 set2) intersection_set);
    ()

  let test_member () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let a_set = insert_list empty a_list in
    (* checks every value in list to see if it's in the set *)
    let rec list_checker list1 my_set =
      match list1 with
      | [] -> true
      | x :: xs -> (member my_set x) && list_checker xs my_set in
    assert (list_checker a_list a_set);
    ()

  (* searches a list and uses C.compare to find a specific element *)
  let search_in_list (elmt : elt) (lst : elt list) : bool =
    List.fold_right lst ~f: (fun a acc -> (C.compare a elmt = Equal) || acc)
		    ~init: false 
 
  (* removes one instance of some element from a list. If the element is not
   * found, then return the normal list *)
  let rec list_remove (elmt : elt) (lst : elt list) (acc: elt list) : elt list =
    match lst with
    | [] -> acc
    | x :: xs -> 
       if (C.compare x elmt = Equal) then acc @ xs
       else list_remove elmt xs ([x] @ acc)
  
  (* checks if two lists are identical *)
  let rec lists_checker (list1 : elt list) (list2 : elt list) : bool =
      match (List.is_empty list1, List.is_empty list2) with
      | (true, true) -> true
      | (false, true) -> false
      | (true, false) -> false
      | (false, false) -> (
	match list1 with
	| [] -> failwith "empty list"
	| x :: xs -> 
	   if search_in_list x list2 
	   then lists_checker xs (list_remove x list2 [])
	   else false)
 
  let test_choose () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let set1 = insert_list empty a_list in
    let rec choose_test (my_set : set) : elt list =
      match (choose my_set) with
      | None -> []
      | Some (x, xs) -> x :: choose_test xs in
    assert (lists_checker (choose_test set1) a_list);
    ()

  (* Removes a string from a stringlist. If the string is not found, then 
   * return the regular list *)
  let rec str_remove (elmt : string) (lst : string list) 
		     (acc: string list) : string list =
    match lst with
    | [] -> acc
    | x :: xs -> 
       if (String.equal x elmt) then acc @ xs
       else str_remove elmt xs ([x] @ acc)

  (* Searches a string list for a sepcific string *)
  let search_str_list (elmt : string) (lst : string list) : bool =
    List.fold_right lst ~f: (fun a acc -> (String.equal a elmt) || acc)
		    ~init: false
 
  (* Checks if two string lists are identical *)
  let rec stringlist_checker list1 list2 =
    match (List.is_empty list1, List.is_empty list2) with
      | (true, true) -> true
      | (false, true) -> false
      | (true, false) -> false
      | (false, false) -> (
	match list1 with
	| [] -> failwith "empty list"
	| x :: xs -> 
	   if search_str_list x list2
	   then stringlist_checker xs (str_remove x list2 [])
	   else false)
  
  let test_fold () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let set1 = insert_list empty a_list in
    let stringify (lst : elt list) : string list = 
      List.map lst ~f: C.string_of_t in
    let fold_function = (fun x acc -> (C.string_of_t x) :: acc) in
    assert (stringlist_checker (stringify a_list) (fold fold_function [] set1));
    ()

  let test_is_empty () = 
      assert (is_empty empty);
      assert (not (is_empty (singleton (C.gen_random ()))));
      assert (not (is_empty (insert (C.gen_random ()) (insert 
        (C.gen_random ()) empty))));
      ()

  let test_singleton () =
    let x = (C.gen_random ()) in
    let s_x = singleton x in
    assert (member s_x x)

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(
    struct
      type key = C.t
      type value = unit
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value () = ""
      let gen_key () = C.gen ()
      let gen_key_gt k () = C.gen_gt k ()
      let gen_key_lt k () = C.gen_lt k ()
      let gen_key_random () = C.gen_random ()
      let gen_key_between k1 k2 () = C.gen_between k1 k2 ()
      let gen_value () = ()
      let gen_pair () = (gen_key (), gen_value ())
  end)

  type elt = D.key
  type set = D.dict


  (* The following functions are organized such that functions required by those
   * lower in the module are placed higher. *)

  let empty = D.empty
  
  (* Matches tuple with another tuple, removing value (b/c it's unit) *)
  let choose d = match D.choose d with
    | None -> None
    | Some (k, _, d) -> Some (k,d)

  (* takes advantage of fact that choose returns None if dict is empty *)
  let is_empty s = (choose s = None)

  let insert x s = D.insert s x ()

  let singleton x = insert x empty

  (* based off the fold in AssocListDict, found in dict.ml *)
  let fold f init d = D.fold (fun k () acc -> f k acc) init d
    
  (* based off the union given in ListSet *)
  let union xs ys = fold insert ys xs

  let member d k = D.member d k

  let intersect xs ys = 
    let rec intersect_helper xs ys =
      match choose xs with
      | None -> empty
      | Some (k, d) -> if member ys k then insert k (intersect_helper d ys)
			  else intersect_helper d ys in
    if (is_empty xs || is_empty ys) then empty else intersect_helper xs ys

  let remove k d = D.remove d k

  (* implement the rest of the functions in the signature! *)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

					 
  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  let test_is_empty () = 
      assert (is_empty empty);
      assert (not (is_empty (singleton (C.gen_random ()))));
      assert (not (is_empty (insert (C.gen_random ()) (insert 
        (C.gen_random ()) empty))));
      ()


  let test_singleton () =
    let x = (C.gen_random ()) in
    let s_x = singleton x in
    assert (member s_x x)

  (* adapted from 'generate_random_list' given in implementation above *)
  let rec generate_list (size : int) : elt list =
      if size <= 0 then []
      else (C.gen_random () :: (generate_list (size - 1)))

  
  let test_insert () =
    let elts = generate_list 100 in

    (* takes a list, inserts elts one by one into a set, then checks set for
     * that insertion and all others preceding it *)
    let rec check_list (list_to_check : elt list) (list_to_insert : elt list)
      (inserted_set : set) : bool =

      (* checks every value in list to see if it's in the set *)
      let rec list_checker list1 my_set =
	match list1 with
	| [] -> true
	| x :: xs -> (member my_set x) && list_checker xs my_set in

      match list_to_insert with
      | [] -> true
      | x :: xs -> 
	 let new_checked_list = x :: list_to_check in
	 let new_inserted_set = insert x inserted_set in
	 (list_checker new_checked_list new_inserted_set) &&
	   (check_list new_checked_list xs new_inserted_set) in

    assert (check_list [] elts empty);
    ()
  
  (* taken from ListSet tests above, this function takes a list of elts and 
   * adds them to the set given in an argument *)  
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d
  

  (* takes a set, removes elts one by one while adding them to a separate elt
   * list, then checking to see if that elt and others removed before it
   * are indeed removed *)
  let test_remove () =
    let elts = generate_list 100 in
  
    let rec check_list (list_to_check : elt list) (list_to_remove : elt list)
      (shrinking_set : set) : bool =

      (* checks every value in list to see if it's not in the set *)
      let rec list_checker list1 my_set =
	match list1 with
	| [] -> true
	| x :: xs -> not(member my_set x) && list_checker xs my_set in

      match list_to_remove with
      | [] -> true
      | x :: xs -> 
	 let new_checked_list = x :: list_to_check in
	 let new_shrinking_set = remove x shrinking_set in
	 (list_checker new_checked_list new_shrinking_set) &&
	   (check_list new_checked_list xs new_shrinking_set) in

    assert (check_list [] elts (insert_list empty elts));
    ()

  (* generates a list of elts of size n all greater than each other, with
   * the first value being elt_init *)
  let rec greater_list (n : int) (elt_init : elt) : elt list =
    if n = 0 then [] else 
      let new_key = C.gen_gt elt_init () in 
      new_key :: (greater_list (n - 1) new_key)

  (* get the last elt in a list *)
  let rec last_elt (lst : elt list) : elt =
    match lst with
    | [] -> failwith "empty list - no last elt"
    | x :: [] -> x
    | _ :: xs -> last_elt xs

  (* checks if two sets have identical *)
  let rec set_compare (shifty_set : set) (good_set : set) : bool =
      match (choose shifty_set, choose good_set) with
      | (None, None) -> true
      | (None, Some _) -> false
      | (Some _, None) -> false
      | (Some (x, xs), _)  -> 
          if member good_set x then set_compare xs (remove x good_set)
	  else false


  (* tests if the set representing the union of two sets is identical to the
   * union put together manually *)
  let test_union () = 
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let b_list = greater_list 100 (last_elt a_list) in
    let c_list = greater_list 100 (last_elt b_list) in
    let set1 = insert_list empty (a_list @ b_list) in
    let set2 = insert_list empty (b_list @ c_list) in
    let union_set = insert_list empty (a_list @ (b_list @ c_list)) in
    assert (set_compare (union set1 set2) union_set);
    ()


  (* tests if the set representing the intersection of two sets is identical
   * to the intersection put together manually *)
  let test_intersect () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let b_list = greater_list 100 (last_elt a_list) in
    let c_list = greater_list 100 (last_elt b_list) in
    let set1 = insert_list empty (a_list @ b_list) in
    let set2 = insert_list empty (b_list @ c_list) in
    let intersection_set = insert_list empty (b_list) in
    assert (set_compare (intersect set1 set2) intersection_set);
    ()
  
  (* makes a set from a list, then uses member to check that every member of the
   * list is in the set with member *)
  let test_member () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let a_set = insert_list empty a_list in
    (* checks every value in list to see if it's in the set *)
    let rec list_checker list1 my_set =
      match list1 with
      | [] -> true
      | x :: xs -> (member my_set x) && list_checker xs my_set in
    assert (list_checker a_list a_set);
    ()
  
  (* searches a list and uses C.compare to find a specific element *)
  let search_in_list (elmt : elt) (lst : elt list) : bool =
    List.fold_right lst ~f: (fun a acc -> (C.compare a elmt = Equal) || acc)
		    ~init: false
  
  (* removes one instance of some element from a list. If the element is not
   * found, then return the normal list *)
  let rec list_remove (elmt : elt) (lst : elt list) (acc: elt list) : elt list =
    match lst with
    | [] -> acc
    | x :: xs -> 
       if (C.compare x elmt = Equal) then acc @ xs
       else list_remove elmt xs ([x] @ acc)
  
  (* checks if two lists are identical *)
  let rec lists_checker (list1 : elt list) (list2 : elt list) : bool =
      match (List.is_empty list1, List.is_empty list2) with
      | (true, true) -> true
      | (false, true) -> false
      | (true, false) -> false
      | (false, false) -> (
	match list1 with
	| [] -> failwith "empty list"
	| x :: xs -> 
	   if search_in_list x list2 
	   then lists_checker xs (list_remove x list2 [])
	   else false)

  (* inserts a list of elts into a set, then uses choose to reassemble the list.
   * Checks if the two lists are identical. *)	 
  let test_choose () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let set1 = insert_list empty a_list in
    let rec choose_test (my_set : set) : elt list =
      match (choose my_set) with
      | None -> []
      | Some (x, xs) -> x :: choose_test xs in
    assert (lists_checker (choose_test set1) a_list);
    ()
  
  (* Removes a string from a stringlist. If the string is not found, then 
   * return the regular list *)
  let rec str_remove (elmt : string) (lst : string list) 
		     (acc: string list) : string list =
    match lst with
    | [] -> acc
    | x :: xs -> 
       if (String.equal x elmt) then acc @ xs
       else str_remove elmt xs ([x] @ acc)

  (* Searches a string list for a sepcific string *)
  let search_str_list (elmt : string) (lst : string list) : bool =
    List.fold_right lst ~f: (fun a acc -> (String.equal a elmt) || acc)
		    ~init: false


  (* Checks if two string lists are identical *)
  let rec stringlist_checker list1 list2 =
    match (List.is_empty list1, List.is_empty list2) with
      | (true, true) -> true
      | (false, true) -> false
      | (true, false) -> false
      | (false, false) -> (
	match list1 with
	| [] -> failwith "empty list"
	| x :: xs -> 
	   if search_str_list x list2
	   then stringlist_checker xs (str_remove x list2 [])
	   else false)

  (* Tests the fold function by comparing the results of converting a list of 
   * elts into a list of strings with List.map and the results of converting a 
   * set of functions into a list of strings *)  
  let test_fold () =
    let a_key = (C.gen_random ()) in
    let a_list = greater_list 100 a_key in
    let set1 = insert_list empty a_list in
    let stringify (lst : elt list) : string list = 
      List.map lst ~f: C.string_of_t in
    let fold_function = (fun x acc -> (C.string_of_t x) :: acc) in
    assert (stringlist_checker (stringify a_list) (fold fold_function [] set1));
    ()
    
  (* add your test functions to run_tests *)
  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end



(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  (* ListSet (C) *)
   DictSet (C)

