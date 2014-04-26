(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors: Madhu Vijay & Michael Patterson
 * psetdict.ml -- adapted from PS5 dict.ml; provides basic dict functionality in
 * the form of an AssocListDict *)

open Core.Std

exception TODO

(* Interfaces for a dictionary, which associates values with keys. *)
module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u. *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. see TESTING EXPLANATION below *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Ordering.t
  val string_of_key : key -> string
  val string_of_value : value -> string

  (* Functions used for testing. *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a key less than the argument. *)
  val gen_key_lt : key -> unit -> key

  (* Generates a key between the two arguments. Return None if no such
   * key exists. *)
  val gen_key_between : key -> key -> unit -> key option

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end



module StringIntDictArg : DICT_ARG =
struct
  type key = string
  type value = int
  let rec compare x y = 
    match String.compare x y with
    | 0 -> Equal
    | -1 -> Less
    | 1 -> Greater
    | _ -> failwith "invalid compare "
  let string_of_key key = key
  let string_of_value = string_of_int
  let gen_key () = "0"
  let gen_key_gt x () = string_of_int ((int_of_string x) + 1)
  let gen_key_lt x () = string_of_int ((int_of_string x) - 1)
  let gen_key_between x y () =
    let int_x = int_of_string x in
    let int_y = int_of_string y in
    let (lower, higher) = (min int_x int_y, max int_x int_y) in
    if higher - lower < 2 then None else Some (string_of_int (higher - 1))

  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> string_of_int (Random.int 10000))

  let gen_value () = 
    let _ = Random.self_init () in
     Random.int 10000

  let gen_pair () = (gen_key_random(), gen_value())
end



(* An association list implementation of our DICT signature. *)
module AssocListDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
struct
  type key = D.key;;
  type value = D.value;;
  type dict = (key * value) list;;

  (* INVARIANT: sorted by key, no duplicates *)

  let empty = [] ;;

  let fold f d = List.fold_left ~f:(fun a (k, v) -> f k v a) ~init:d

  let rec lookup d k =
    match d with
      | [] -> None
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Equal -> Some v1
          | Greater -> lookup d1 k
          | _ -> None)

  let member d k =
    match lookup d k with
      | None -> false
      | Some _ -> true

  let rec insert d k v =
    match d with
      | [] -> [(k,v)]
      | (k1,v1)::d1 ->
        (match D.compare k k1 with
          | Less -> (k,v)::d
          | Equal -> (k,v)::d1
          | Greater -> (k1,v1)::(insert d1 k v))

  let rec remove d k =
    match d with
      | [] -> []
      | (k1,v1)::d1 ->
	(match D.compare k k1 with
          | Equal -> d1
          | Greater -> (k1,v1)::(remove d1 k)
          | _ -> d)

  let choose d =
    match d with
      | [] -> None
      | (k,v)::rest -> Some(k,v,rest)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    let f = (fun y (k,v) -> y ^ "\n key: " ^ D.string_of_key k ^
      "; value: (" ^ D.string_of_value v ^ ")") in
    List.fold_left ~f:f ~init:"" d

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left lst ~f:(fun r (k,v) -> insert r k v) ~init:d

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right lst ~f:(fun (k,v) r -> insert r k v) ~init:d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* I have commented this out because it is not used in my tests. The compiler
   * was throwing warnings. *)
  (* generates a (key,value) list with keys in random order *)
  (*
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))
   *)


  (* Note: Many of the following tests were adapted directly from the tests
   * written for BTDict below. *)

  (* Tests insert, lookup, and member by adding 200 elements to each of 2 dicts
   * and recursively checking that "member" returns true for each key, and that
   * "lookup" returns the correct value. Adapted from distribution code. *)
  let test_insert_lookup_member () =
    let pairs1 = generate_pair_list 200 in
    let d1 = insert_list empty pairs1 in
    let f d = fun (k,v) -> assert(member d k && lookup d k = Some v) in
    List.iter pairs1 ~f:(f d1);
    (* Tests insertion in the reverse order. *)
    let d2 = insert_list_reversed empty pairs1 in
    List.iter pairs1 ~f:(f d2)

  let test_remove () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          )
      );
    ()

  (* Tests the choose function by making sure that (1) choosing 200 elements
   * from a 200-element dict yields an empty dict, and (2) every element that
   * is chosen is inside the original dict, but not inside the new dict that
   * is returned by choose. *)
  let test_choose () =
    let rec test_choose_rec (d : dict) (num : int) : bool =
      match choose d with
      | None -> num = 0
      | Some (k, v, d') ->
        lookup d k = Some v && lookup d' k = None
                            && test_choose_rec d' (num - 1)
    in
    let size = 200 in
    let pairs = generate_pair_list size in
    let d = insert_list empty pairs in
    assert(test_choose_rec d size)

  (* Tests fold using a lookup-based function. This test is fairly
   * comprehensive because it runs lookup on every element (using fold)
   * to ensure that fold is working properly. *)
  let test_fold () =
    let pairs2 = generate_pair_list 500 in
    let d2 = insert_list empty pairs2 in
    assert(fold (fun k v b -> lookup d2 k = Some v && b) true d2)

  (* Runs all the tests from above *)
  let run_tests () =
    test_insert_lookup_member() ;
    test_remove() ;
    test_choose() ;
    test_fold() ;
    ()

end

module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  AssocListDict(D)
