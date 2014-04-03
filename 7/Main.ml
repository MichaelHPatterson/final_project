open Core.Std
open Event51

(* Generating ponds *)
let num_ponds = 12
let pond_size = 15
let gen_ponds () : unit =
  World.spawn_iter num_ponds pond_size
                   (fun () -> ())
                   (fun p -> ignore (new Pond.pond p))

(* Generating towns *)
let num_towns = 20
let town_size = 20
let gen_towns () : unit =
  let gold_id = ref (-1) in
  World.spawn_iter num_towns town_size
                   (fun () -> gold_id := Town.get_next_gold_id ())
                   (fun p -> ignore (new Town.town p (* !gold_id *)))

let gen_dany () =
  new Dany.dany (0,0)

let gen_wall () =
  new Wall.wall (World.size-1,World.size-1)

let gen_dragon () =
  new Dragon.dragon (0,0)

let gen_white_walker () =
  new WhiteWalker.white_walker (World.size-1,World.size-1)

let gen_city () =
  (* Don't ignore, since we will need to pass the city to some other objects. *)
  new KingsLanding.kings_landing (World.size/2,World.size/2)

(* Initializer functions *)
let part1_initializer () : unit =
  ignore (new Pond.pond (0,0));
  ignore (new Town.town (1,1));
  ignore (new KingsLanding.kings_landing (2,2));
  ignore (new Human.human (3,3));
  ignore (new Dany.dany (4,4));
  ignore (new Dragon.dragon (5,5));
  ignore (new Wall.wall (6,6));
  ignore (new WhiteWalker.white_walker (7,7))

(* DO NOT TOUCH ANYTHING BELOW THIS POINT UNTIL THE PARTS ARE RELEASED *)

let part2_initializer () : unit =
  ignore (gen_city ());
  ignore (new Human.human (World.size/2+1,World.size/2));
  ignore (gen_dragon ());
  ignore (gen_white_walker ())

let part3_initializer () : unit =
  ignore (gen_city ());
  ignore (gen_dany ());
  ignore (gen_wall ());
  ignore (gen_ponds ());
  ignore (gen_towns ());

  let count = ref 20 in
  while !count > 0 do
    ignore (new Human.human (World.size/2+1,World.size/2));
    count := !count - 1
  done;

  ignore(gen_dragon ());
  ignore(gen_white_walker ())

let part4_initializer () : unit =
  ignore (gen_city ());
  ignore (gen_ponds ());
  ignore (gen_towns ())

let final_initializer () : unit =
  ignore (gen_city ());
  ignore (gen_dany ());
  ignore (gen_wall ());
  ignore (gen_ponds ());
  ignore (gen_towns ())

(* Function that is called continuously while the simulation is running. *)
let event_loop part () : unit =
  Graphics.clear_graph () ;
  if part >= 2 then Event51.fire_event World.move_event () ;
  if part >= 3 then Event51.fire_event World.action_event () ;
  if part >= 4 then Event51.fire_event World.age_event () ;
  (* draw loop *)
  World.indices begin fun p ->
    let sorted = List.sort ~cmp:(fun x y -> compare x#draw_z_axis y#draw_z_axis)
                           (World.get p)
    in
    List.iter ~f:(fun w -> w#draw) sorted
  end

(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : (unit -> unit) * int =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "part1" -> part1_initializer, 1
  | "part2" -> part2_initializer, 2
  | "part3" -> part3_initializer, 3
  | "part4" | "part5" -> part4_initializer, 4
  | "final" | "part6" -> final_initializer, 6
  | _ -> usage ()


let run () : unit =
  let initialize, part = parse_args () in
  UI.run_world initialize (event_loop part)
;;

run () ;;
