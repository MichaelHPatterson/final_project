open Core.Std

(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  In our case, we will
 * be using a dictionary to build an index for the web, associating
 * a set of URLs with each word that we find as we crawl the web.
 *)
exception TODO

module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u.
   * Our reducing function f must have the type:
   *      key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   *
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
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

  (* Use these functions for testing. See TESTING EXPLANATION. *)

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



(* An example implementation of our DICT_ARG signature. Use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_lt x () = x - 1
  let gen_key_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
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

  (****************************************************************)
  (* Tests for our AssocListDict functor                          *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

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

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup d1 k = Some v)) ;
    ()

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

  let test_lookup () =
    ()

  let test_choose () =
    ()

  let test_member () =
    ()

  let test_fold () =
    ()

  let run_tests () =
    test_insert() ;
    test_remove() ;
    test_lookup() ;
    test_choose() ;
    test_member() ;
    test_fold() ;
    ()

end



(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. *)
  type pair = key * value

  (* Type definition for dictionary, as a 2-3 Tree. *)
  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* For removal: A direction will distinguish which configuration we came from
   * in the removal cases. We use direction2 when the parent is a 2-node, and
   * direction3 if the parent is a 3-node. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  (* Represents an empty dictionary as an empty 2-3 tree *)
  let empty : dict = Leaf

  (* Reduce the dictionary based on a function f and a base case u.
   * Returns f k1 v1 (f k2 v2 (f ... (f kn vn u))). *)
  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (left, (k, v), right) -> fold f (fold f (f k v u) right) left
    | Three (left, (k1, v1), mid, (k2, v2), right) -> 
      fold f (fold f (f k1 v1 (fold f (f k2 v2 u) right)) mid) left

  (* Uses the string_of_key and string_of_value functions from D. *)
  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value

  (* Converts a dictionary to a string, based on the example from AssocListDict
   * above. *)
  let string_of_dict (d: dict) : string =
    let f = (fun k v curr -> curr ^ "\n key: " ^ string_of_key k ^
      "; value: (" ^ string_of_value v ^ ")") in
    fold f "" d


  (* Debugging function that prints out the structure of the tree in text
   * format. *)
  let rec string_of_tree (d: dict) : string =
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ ","
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  (* Handles the upward phase for w when the parent is a Two node with
   * key-value pair x and other child x_other. *)
  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =
    let (wk, _) = w in
    let (xk, _) = x in
    if D.compare wk xk = Greater then
      Done (Three (x_other, x, w_left, w, w_right))
    else Done (Three (w_left, w, w_right, x, x_other))

  (* Handles the upward phase for w when the parent is a Three node with
   * key-value pairs x and y and other children other_left and other_right. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (wk, _) = w in
    let (yk, _) = y in
    (* Using two if/then/elses instead of a combined match avoids making an
     * unnecessary comparison in some cases. *)
    if D.compare wk yk = Greater then
      let left = Two (other_left, x, other_right) in
      let right = Two (w_left, w, w_right) in
      Up (left, y, right)
    else
      let (xk, _) = x in
      if D.compare wk xk = Greater then
        let left = Two (other_left, x, w_left) in
        let right = Two (w_right, y, other_right) in
        Up (left, w, right)
      else
        let left = Two (w_left, w, w_right) in
        let right = Two (other_left, y, other_right) in
        Up (left, x, right)

  (* Downward phase for inserting (k,v) into our dictionary d.
   * The downward phase returns a "kicked" up configuration, where
   *
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   *
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d.
   *
   * The functions insert_downward, insert_downward_two, and
   * insert_downward_three are __mutually recursive__, hence the
   * "let rec" and the "and" keywords. Here, we use three mutually recursive
   * functions to simplify our code into smaller pieces.
   *
   * Two functions f and g are __mutually recursive__ if in f's definition,
   * f calls g, and in g's definition, g calls f. This definition of
   * mutually recursive definitions can be extended to more than two functions,
   * as follows:
   *
   * Functions f1, f2, f3, ..., fn are mutually recursive if for each of
   * these functions f, all of the other f_i's can be called on some execution
   * of f. *)

  (* Handles the downward phase of inserting (k,v) into the dictionary. Calls
   * insert_downward_two or insert_downward_three as necessary. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
    | Leaf -> Up (Leaf, (k, v), Leaf)
    | Two(left,n,right) -> insert_downward_two (k,v) n left right
    | Three(left,n1,middle,n2,right) ->
      insert_downward_three (k,v) n1 n2 left middle right

  (* Handles the downward phase on a Two node, to insert (k, v) into a
   * dictionary Two(left, (k1,v1), right). Calls insert_downward and
   * insert_upward_two where needed. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    let (dict_insert, dict_other, inserted_side) =
      if D.compare k k1 = Greater then (right, left, "r")
      else (left, right, "l")
    in
    match insert_downward dict_insert k v with
    | Done d ->
      if inserted_side = "r" then Done (Two (dict_other, (k1,v1), d))
      else Done (Two (d, (k1,v1), dict_other))
    | Up (w_left, w, w_right) ->
      insert_upward_two w w_left w_right (k1,v1) dict_other


  (* Handles the downward phase on a Three node, to insert (k, v) into a
   * dictionary Three(left, (k1,v1), middle, (k2,v2), right). Calls
   * insert_downward and insert_upward_three when needed. *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    let (dict_insert, other_left, other_right, inserted_side) =
      if D.compare k k2 = Greater then (right, left, middle, "r")
      else if D.compare k k1 = Greater then (middle, left, right, "m")
      else (left, middle, right, "l")
    in
    match insert_downward dict_insert k v with
    | Done d ->
      if inserted_side = "r" then Done(Three(left,(k1,v1),middle,(k2,v2),d))
      else if inserted_side = "m" then Done(Three(left,(k1,v1),d,(k2,v2),right))
      else Done(Three(d,(k1,v1),middle,(k2,v2),right))
    | Up (wl, w, wr) ->
      insert_upward_three w wl wr (k1,v1) (k2,v2) other_left other_right

  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node.
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r)
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d)
      | Right2,z,Three(a,x,b,y,c),d -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d))) (* CHECK THISSSSSSSSSSSSSSSSSSSSS *)
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d)
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e)
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Equal -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Equal, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Equal -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  (* DO NOT EDIT THIS *)
  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Equal, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  (* DO NOT EDIT THIS *)
  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  (* Returns the value of the given key in our dictionary as an option, or
   * None if the key is not in the dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two (d1, (k', v'), d2) ->
      (match D.compare k k' with
      | Equal -> Some v'
      | Less -> lookup d1 k
      | Greater -> lookup d2 k)
    | Three (d1, (k1, v1), d2, (k2, v2), d3) ->
      (match D.compare k k1 with
      | Equal -> Some v1
      | Less -> lookup d1 k
      | Greater ->
        (match D.compare k k2 with
        | Equal -> Some v2
	| Less -> lookup d2 k
	| Greater -> lookup d3 k))

  (* Returns true if the key k is in the dictionary d. *)
  let member (d: dict) (k: key) : bool = not (lookup d k = None)
  
  (* Chooses a pair from the top of the dictionary, and returns the pair's
   * key and value and the rest of the dictionary as an option; or returns
   * None if the dictionary is empty. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two (_, (k,v), _) | Three (_, (k,v), _, _, _) -> Some (k, v, remove d k)

  (* How are you testing that you tree is balanced?
   * ANSWER:
   *    Checks whether a tree is balanced, by counting the height of the tree
   *    along each branch, and making sure all of the numbers match up. Uses
   *    a recursive helper function that returns None if any disparity is
   *    found.
   *)
  let balanced (d: dict) : bool =
    let rec b_count (dict1: dict) : int option =
      match dict1 with
      | Leaf -> Some 0
      | Two (left, _, right) -> (
	match (b_count left, b_count right) with
	| (None, _) | (_, None) -> None
	| (Some x, Some y) -> if x = y then Some (x + 1) else None)
      | Three (left, _, mid, _, right) -> (
	match (b_count left, b_count mid, b_count right) with
	| (None, _, _) | (_, None, _) | (_, _, None) -> None
	| (Some x, Some y, Some z) -> 
          if (x = y && y = z) then Some (x + 1) else None) in
    not (b_count d = None)


  (********************************************************************)
  (*       TESTS                                                      *)
  (* You must write more comprehensive tests, using our remove tests  *)
  (* below as an example                                              *)
  (********************************************************************)

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

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))


  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    ()

  (* Tests insert, member, and lookup by generating 2 lists of 100 pairs,
   * creating 3 trees with them (by adding elements in increasing, decreasing,
   * and random order); checking that eachh tree is balanced, calling the
   * member function to make sure that each dictionary contains the correct
   * elements, and calling lookup to check that each key matches its value. *)
  let test_insert_member_lookup () =
    let pairs1 = generate_pair_list 100 in 
    let d1 = insert_list empty pairs1 in
    let d2 = insert_list_reversed empty pairs1 in
    let pairs2 = generate_random_list 100 in
    let d3 = insert_list empty pairs2 in
    assert(balanced d1 && balanced d2 && balanced d3);
    let rec check (pairs : pair list) (d : dict) : unit =
      match pairs with
      | [] -> ()
      | (k,v) :: pairs' ->
        assert(member d k);
        assert(lookup d k = Some v);
        check pairs' d
    in
    check pairs1 d1;
    check pairs1 d2;
    check pairs2 d3;
    ()

  let test_remove_simple () =
    let pairs = generate_pair_list 13 in
    let d = insert_list_reversed empty pairs in
    let rec traverse (pairs : pair list) (d : dict) : unit =
      match pairs with
      | [] -> ()
      | (k,_) :: pairs' ->
        let d = remove d k in
        assert(not (member d k));
        assert(balanced d);
        traverse pairs' d
    in
    traverse pairs d

  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup r2 k = Some v)) ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 12 in
    let d1 = insert_list empty pairs1 in
    assert(balanced d1);
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          )
        in
        assert(balanced r)
      );
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1 ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) in
        assert(balanced r)
      ) ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right pairs5 ~f:(fun (k,_) d -> remove d k) ~init:d5 in
    List.iter pairs5 ~f:(fun (k,_) -> assert(not (member r5 k)));
    assert(r5 = empty);
    assert(balanced r5);
    ()

  let test_fold () =
    let _ = Random.self_init () in
    let size = Random.int 10000 in
    let pairs = generate_random_list size in
    let d = insert_list empty pairs in
    print_endline (string_of_tree d);
    let length (d : dict) : int = fold (fun _ _ n -> n + 1) 0 d in
    assert(length d = size);
    

  let run_tests () =
    test_balance();
    test_insert_member_lookup();
    test_fold();
    test_remove_simple();
    test_remove_nothing();
    test_remove_from_nothing();
    test_remove_in_order();
    test_remove_reverse_order();
    test_remove_random_order();
    ()

end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * AssocListDict functor and run the tests *)
module IntStringListDict = AssocListDict(IntStringDictArg) ;;
IntStringListDict.run_tests();;

(* Create a dictionary mapping ints to strings using our
 * BTDict functor and run the tests.
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 tree implementation. *)
module IntStringBTDict = BTDict(IntStringDictArg);;
IntStringBTDict.run_tests();;




(******************************************************************)
(* Make: a functor that creates a DICT by calling our             *)
(* AssocListDict or BTDict functors                               *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  BTDict(D)

