(* CS51 Final Project: N x N Matching
 * CS51 Spring 2014
 * Authors: Madhu Vijay & Michael Patterson
 * dict.ml -- provides dict functionality for use in reading from files; adapted
 * from PS5 but with new AVLDict, which uses an AVL tree *)

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
  val key_of_string : string -> key
  val val_of_int : int -> value
  val int_of_val : value -> int

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
  val key_of_string : string -> key
  val val_of_int : int -> value
  val int_of_val : value -> int

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


(* The module that will be used for our owner and elt dicts. Owners and elts
 * will be stored as strings, and their corresponding indices in the ranking
 * matrix will be stored as ints. *)
module StringIntDictArg : DICT_ARG =
struct
  type key = string
  type value = int
  let compare x y = 
    (* Less means that x comes alphabetically before y; Greater means that it
     * comes after *)
    match String.compare x y with
    | 0 -> Equal
    | -1 -> Less
    | 1 -> Greater
    | _ -> failwith "invalid compare"
  let string_of_key key = key
  let string_of_value = string_of_int
  let key_of_string my_string = my_string
  let val_of_int my_int = my_int
  let int_of_val my_int = my_int
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


(* Directly copied from the AssocListDict in PS5. Not used in our final
 * implementation, but was useful for testing purposes. "An association list 
 * implementation of our DICT signature." *)
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
  let key_of_string = D.key_of_string
  let val_of_int = D.val_of_int
  let int_of_val = D.int_of_val

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


(* A dictionary of an AVL tree, a self-balancing binary search tree that offers
 * n*log(n) runtime. NOTE: Has an invariant whereby insertions of keys already
 * in the dictionary are not required *)
module AVLDict(D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
struct
  type key = D.key
  type value = D.value
  (* A dictionary entry is a (key, value) pair. *)
  type pair = key * value

  (* Type definition for dictionary as a BST. Dicts are either a leaf or a pair
   * with two branches, which could include up to two leaves. Also stores its
   * height. *)
  type dict = 
    | Leaf
    | Two of int * dict * pair * dict
  
  (* empty dict is just a leaf *)
  let empty : dict = Leaf

  let _string_of_key = D.string_of_key
  let _int_of_val = D.int_of_val

  (* folds over the whole function, starting with the top node of the tree and
   * then moving down, evaluating right branches of Two Branches before left
   *  branches *)
  let rec fold (f : key -> value -> 'a -> 'a) (init : 'a) (d : dict) : 'a =
    match d with
    | Leaf -> init
    | Two (_, left, (k, v), right) -> fold f (fold f (f k v init) right) left

  (* looks up a specific key and returns its value, or None if the key is not
   * in the dict *)
  let rec lookup (d : dict) (k : key) : value option =
    match d with
    | Leaf -> None
    | Two (_, left, (k1, v1), right) ->
       (match D.compare k k1 with
	| Equal -> Some v1
	| Less -> lookup left k
	| Greater -> lookup right k)
  
  (* Given two pairs, returns the comparison of their keys *)
  let k_compare p1 p2 =
    let (x, _) = p1 in
    let (y, _) = p2 in
    D.compare x y

  (* returns true if key is in dict, false if it's not *)
  let member d k = (lookup d k) <> None

  (* given the left and right branches of a tree as arguments, returns the
   * height of the tree, i.e. the max height b/t left and right branches + 1 *)
  let rec get_height (d1 : dict) (d2 : dict) : int = 
    match (d1, d2) with
    | (Leaf, Leaf) -> 1
    | (Leaf, Two (_, left, _, right)) -> (get_height left right) + 1
    | (Two (_, left, _, right), Leaf) -> (get_height left right) + 1
    | (Two (_, left1, _, right1), Two (_, left2, _, right2)) ->
       (max (get_height left1 right1) (get_height left2 right2)) + 1

  (* finds height of tree from its data structure *)
  let extract_height (d : dict) : int = 
    match d with
    | Leaf -> 0
    | Two (x, _, _, _) -> x

  (* returns the difference in height of a tree's left and right branches; used
   * to check for balanced-ness *)
  let balance_factor (my_dict : dict) : int =
      match my_dict with
      | Leaf -> failwith "got Leaf in balance_factor"
      | Two (_, left, _, right) -> (extract_height left)-(extract_height right)

  (* balances the tree by matching a tree with one of four unbalanced instances.
   * For more information on how this works, see photo in our documentation. *)
  let balance (d : dict) : dict =
    let left_left (input : dict) : dict =
      match input with
      | Leaf -> failwith "got Leaf in left_left"
      | Two(my_height, l_dict, my_pair, r_dict) ->
	 (match l_dict with
	 | Leaf -> failwith "got Leaf in left_left"
	 | Two(_, l_ldict, l_pair, l_rdict) ->
	 Two(my_height-1, l_ldict, l_pair, Two(my_height-2, l_rdict, my_pair, 
           r_dict))) in
    
    let left_right (input : dict) : dict =
      match input with
      | Leaf -> failwith "got Leaf in left_right"
      | Two(my_height, l_dict, my_pair, r_dict) ->
	 (match l_dict with
	  | Leaf -> failwith "got Leaf in left_right"
	  | Two(_, l_ldict, l_pair, l_rdict) ->
	     (match l_rdict with
	      | Leaf -> failwith "got Leaf in left_right"
	      | Two (_, l_r_ldict, l_rpair, l_r_rdict) ->
	 Two(my_height, Two(my_height-1, Two(my_height-2, l_ldict, l_pair,
           l_r_ldict), l_rpair, l_r_rdict), my_pair, r_dict))) in

    let right_right (input : dict) : dict =
      match input with
      | Leaf -> failwith "got Leaf in right_right"
      | Two(my_height, l_dict, my_pair, r_dict) ->
	 (match r_dict with
	  | Leaf -> failwith "got Leaf in right_right"
	  | Two(_, r_ldict, r_pair, r_rdict) ->
	 Two(my_height-1, Two(my_height-2, l_dict, my_pair, r_ldict), r_pair,
           r_rdict)) in

    let right_left (input : dict) : dict =
      match input with
      | Leaf -> failwith "got Leaf in right_left"
      | Two(my_height, l_dict, my_pair, r_dict) ->
	 (match r_dict with
	  | Leaf -> failwith "got Leaf in right_left"
	  | Two(_, r_ldict, r_pair, r_rdict) ->
	     (match r_ldict with
	      | Leaf -> failwith "got Leaf in right_left"
	      | Two(_, r_l_ldict, r_lpair, r_l_rdict) ->
	 Two(my_height, l_dict, my_pair, Two(my_height-1, r_l_ldict, r_lpair,
           Two(my_height-2, r_l_rdict, r_pair, r_rdict))))) in

    match d with
    | Leaf -> d
    | Two(_, dict_left, _, dict_right) ->
       (match balance_factor d with
	| 2 -> (
	  let bal_left = balance_factor dict_left in
	  if bal_left = -1 then left_left(left_right d)
	  else (if (bal_left = 0 || bal_left = 1) then left_left d
		else failwith "invalid balance_factor"))
	| -2 -> (
	  let bal_right = balance_factor dict_right in
	  if bal_right = 1 then right_right(right_left d)
	  else (if (bal_right = 0 || bal_right = -1) then right_right d
		else failwith "invalid balance_factor"))
	| x -> if (x < 3 && x > -3) then d else failwith "balance_factor out of
						      bounds")
  
  (* inserts a key,value pair into a dict, balancing the tree as the key is 
   * inserted *)
  let insert (d : dict) (k : key) (v : value) : dict = 
    let rec insert_help (d : dict) (p : pair) : dict =
      match d with
      | Leaf -> Two(1, Leaf, p, Leaf)
      | Two (_, left, p1, right) -> 
	 (match k_compare p p1 with
	  | Equal -> failwith "already in dict"
	  | Less -> (
	    let new_left = insert_help left p in
	    balance (Two(get_height new_left right, new_left, p1, right)))
	  | Greater -> (
	    let new_right = insert_help right p in
	    balance (Two(get_height left new_right, left, p1, new_right)))) in
    insert_help d (k,v)

  (* In the interest of time, this remove function has been coded inefficiently.
   * Our program will not require the removal of entries from a dictionary, so
   * this is meant simply to appease the requirements of the signature. *)
  let remove (d : dict) (my_key : key) : dict =
    fold (fun k v acc -> if (D.compare my_key k = Equal) then acc
			 else insert acc k v) empty d

  (* Also coded in the interest of time, because this has no use in our program
   * and is only coded for appeasing the sig *)
  let choose (d : dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two (_, _, (k, v), _) -> Some (k, v, remove d k)

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d : dict) : string = 
    let f = (fun k v curr -> curr ^ "\n key: " ^ string_of_key k ^
      "; value: (" ^ string_of_value v ^ ")") in
    fold f "" d
  let key_of_string = D.key_of_string
  let val_of_int = D.val_of_int
  let int_of_val = D.int_of_val

  (* a function that checks the balancing of a tree for testing purposes *)
  let rec is_balanced (d : dict) : bool = 
    match d with
    | Leaf -> true
    | Two (_, left, _, right) -> let factor = balance_factor d in
        (factor < 2 && factor > -2) && (is_balanced left) && (is_balanced right)

  (* a depth-first traversal function that is used for testing of the insert
   * function in run_tests *)
  let rec traverse (d : dict) : 'a list option =
    match d with
    | Leaf -> None
    | Two (_, left, (k,_), right) -> 
       (match (traverse left, traverse right) with
	| (None, None) -> Some [k]
	| (None, Some x) -> Some (k :: x)
	| (Some x, None) -> Some (x @ [k])
	| (Some x, Some y) -> Some ((x @ [k]) @ y))

  (* converts the comparison function to output bool *)
  let bool_compare (a : 'a) (b : 'a) : bool = not (D.compare a b <> Equal)

  (* converts the compare function to output ints *)
  let int_compare (a : 'a) (b : 'a) : int =
    match D.compare a b with
    | Less -> -1
    | Equal -> 0
    | Greater -> 1

  (* runs various tests related to AVL tree *)
  let run_tests () : unit =
    let rec list_gen (num : int) (lst : key list) : key list =
      if num <= 0 then lst
      else (let rand_val = D.gen_key_random () in
	   if not (List.mem ?equal:(Some bool_compare) lst rand_val) 
	   then list_gen (num - 1) (rand_val :: lst)
	   else list_gen num lst) in
    let add_list_to (lst : key list) : dict =
      List.fold_right lst ~f:(fun x acc -> insert acc x (D.gen_value ()))
	~init:empty in
    let print_list (lst : key list) : unit =
      List.iter lst ~f:(
        fun x -> Out_channel.output_string stdout ((string_of_key x) ^ "\n");
		 Out_channel.flush stdout) in
    for _i = 0 to 50 do
      let rand_list = list_gen 50 [] in
      if is_balanced (add_list_to (rand_list)) then ()
      else print_list rand_list;
      let sorted_list = List.sort ~cmp:int_compare rand_list in
      let traverse_list = match traverse (add_list_to (rand_list)) with
	| None -> []
	| Some x -> x in
      if sorted_list = traverse_list then ()
      else print_list rand_list;
    done
end

(* Make will return the dict specified below, allowing for the dict to be easily
 * changed *)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  AVLDict(D);;

module StringIntDict = Make(StringIntDictArg);;
