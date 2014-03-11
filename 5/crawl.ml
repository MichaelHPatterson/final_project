open Core.Std
open Util
open CrawlerServices
open Order
open Pagerank


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  if n = 0 then d else (
  if LinkSet.is_empty(frontier) then d else (
  let set_val my_set = 
    match LinkSet.choose my_set with
    | None -> failwith "empty set"
    | Some (x, y) -> (x, y) in
  let add_to_dict my_dict word_list page_url =
    List.fold_right word_list ~f:(fun x acc -> 
     match lookup acc x with
     | None -> WordDict.insert acc x page_url
     | Some x' -> WordDict.insert acc x (LinkSet.insert page_url x'))
     ~init:my_dict in
  let add_to_frontier my_frontier link_list my_visited =
    List.fold_right link_list ~f:(fun x acc ->
      if LinkSet.member my_visited x then acc
      else LinkSet.insert x acc) ~init:my_frontier in
  match CrawlerServices.get_page (fst (set_val frontier)) with
	| None -> failwith "page not found"
	| Some {url = x; links = y; words = z} -> 
	   crawl (n-1) (add_to_frontier frontier y visited)
		 (LinkSet.insert x visited) (add_to_dict d z x)
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
