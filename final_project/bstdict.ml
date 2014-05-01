module BSTDict =
struct
  type key = int
  type value = string
  (* A dictionary entry is a (key, value) pair. *)
  type pair = key * value

  (* Type definition for dictionary as a BST *)
  type dict = 
    | Leaf of int
    | Two of int * dict * pair * dict

  let k_compare p1 p2 =
    let (x, _) = p1 in
    let (y, _) = p2 in
    if x < y then Less else if x > y then Greater else Equal

  let empty : dict = Leaf 0

  let rec get_height (d1 : dict) (d2 : dict) : int = 
    match (d1, d2) with
    | (Leaf _, Leaf _) -> 1
    | (Leaf _, Two (_, left, _, right)) -> (get_height left right) + 1
    | (Two (_, left, _, right), Leaf _) -> (get_height left right) + 1
    | (Two (_, left1, _, right1), Two (_, left2, _, right2)) ->
       (max (get_height left1 right1) (get_height left2 right2)) + 1

  let extract_height (d : dict) : int = 
    match d with
    | Leaf _ -> 0
    | Two (x, _, _, _) -> x

  let is_leaf (d : dict) : bool =
    match d with
    | Leaf _ -> true
    | _ -> false

  let balance_factor (my_dict : dict) : int =
    if is_leaf my_dict then failwith "got Leaf in balance_factor" else
      let Two (_, left, _, right) = my_dict in
      (extract_height left) - (extract_height right)

  let balance (d : dict) : dict =
    let left_left (input : dict) : dict =
      if is_leaf input then failwith "got Leaf in left_left" else
      let Two (my_height, l_dict, my_pair, r_dict) = input in
      let Two (_, l_ldict, l_pair, l_rdict) = l_dict in
      Two(my_height-1, l_ldict, l_pair, Two(my_height-2, l_rdict, my_pair, 
        r_dict)) in
    
    let left_right (input : dict) : dict =
      if is_leaf input then failwith "got Leaf in left_right" else
      let Two (my_height, l_dict, my_pair, r_dict) = input in
      let Two (_, l_ldict, l_pair, l_rdict) = l_dict in
      let Two (_, l_r_ldict, l_rpair, l_r_rdict) = l_rdict in
      Two(my_height, Two(my_height-1, Two(my_height-2, l_ldict, l_pair,
        l_r_ldict), l_rpair, l_r_rdict), my_pair, r_dict) in

    let right_right (input : dict) : dict =
      if is_leaf input then failwith "got Leaf in right_right" else
      let Two (my_height, l_dict, my_pair, r_dict) = input in
      let Two (_, r_ldict, r_pair, r_rdict) = r_dict in
      Two(my_height-1, Two(my_height-2, l_dict, my_pair, r_ldict), r_pair,
        r_rdict) in

    let right_left (input : dict) : dict =
      if is_leaf input then failwith "got Leaf in right_left" else
      let Two (my_height, l_dict, my_pair, r_dict) = input in
      let Two (_, r_ldict, r_pair, r_rdict) = r_dict in
      let Two (_, r_l_ldict, r_lpair, r_l_rdict) = r_ldict in
      Two(my_height, l_dict, my_pair, Two(my_height-1, r_l_ldict, r_lpair,
        Two(my_height-2, r_l_rdict, r_pair, r_rdict))) in

    if is_leaf d then d else (
    let Two (dict_height, dict_left, dict_pair, dict_right) = d in
    match balance_factor d with
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
    
  let rec insert (d : dict) (p : pair) : dict =
    match d with
    | Leaf _ -> Two(1, Leaf 0, p, Leaf 0)
    | Two (_, left, p1, right) -> 
       (match k_compare p p1 with
	| Equal -> failwith "already in dict"
	| Less -> (
	   let new_left = insert left p in
	   balance (Two(get_height new_left right, new_left, p1, right)))
	| Greater -> (
	   let new_right = insert right p in
	   balance (Two(get_height left new_right, left, p1, new_right))))

  let rec is_balanced (d : dict) : bool = 
    match d with
    | Leaf _ -> true
    | Two (_, left, _, right) -> let factor = balance_factor d in
        (factor < 2 && factor > -2) && (is_balanced left) && (is_balanced right)

  let run_tests () : unit =
    let rand : int -> int = Random.self_init () ; Random.int in
    let rec list_gen (num : int) (lst : int list) : int list =
      if num <= 0 then lst
      else (let rand_num = rand 250 in
	   if not (List.mem lst rand_num) 
	   then list_gen (num - 1) (rand_num :: lst)
	   else list_gen num lst) in
    let add_list_to (lst : int list) : dict =
      List.fold_right lst ~f:(fun x acc -> insert acc (x, "")) ~init:empty in
    let print_list (lst : int list) : unit =
      List.iter lst (
        fun x -> Out_channel.output_string stdout (string_of_int x ^ "\n");
		 Out_channel.flush stdout) in
    for i = 0 to 50 do
      let rand_list = list_gen 50 [] in
      if is_balanced (add_list_to (rand_list)) then ()
      else print_list rand_list
    done
end
