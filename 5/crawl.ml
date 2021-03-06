(* PS5
 * CS51 Spring 2014
 * Authors: Madhu Vijay & Michael Patterson
 * Part 1: Crawler -- Implements a crawler
 *)

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
    let gen_key_gt _ () = gen_key ()
    let gen_key_lt _ () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between _ _ () = None
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

(* Builds an index by removing a link from the frontier, visiting the link,
 * adding its outgoing links to the frontier, and updatingn the index so all
 * words on the page are mapped to linksets containing the url. Keeps
 * crawling until the frontier is empty, or we've reached the maximum number of
 * of links n. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  
  if n = 0 then d else (
  if LinkSet.is_empty(frontier) then d else (
  let add_to_dict my_dict word_list page_url =
    List.fold_right word_list ~f:(fun x acc -> 
     match WordDict.lookup acc x with
     | None -> WordDict.insert acc x (LinkSet.singleton page_url)
     | Some x' -> WordDict.insert acc x (LinkSet.insert page_url x'))
     ~init: my_dict in
  let add_to_frontier url_current my_frontier link_list my_visited =
    List.fold_right link_list ~f:(fun x acc ->
      if (LinkSet.member my_visited x) || (url_current = x) then acc
      else LinkSet.insert x acc) ~init:my_frontier in
  let rec find_pages my_set =
    match LinkSet.choose my_set with
    | None -> d
    | Some (x, y) -> (
      match CrawlerServices.get_page x with
      | None -> find_pages y
      | Some {url = my_url; links = y'; words = z'} -> 
	   crawl (n - 1) (add_to_frontier my_url y y' visited)
		 (LinkSet.insert my_url visited) (add_to_dict d z' my_url)) in
  find_pages frontier))
;;

(* Calls the crawl function above to crawl with the appropriate parameters *)
let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;
