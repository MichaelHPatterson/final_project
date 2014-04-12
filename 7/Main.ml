(* Partner 1's name: Madhusudan ("Madhu") Vijay
 * Partner 1's code.seas account: mvijay
 *
 * Partner 2's name: Michael Patterson
 * Partner 2's code.seas account: michaelpatterson
 *
 * CS51, Problem Set 7 (Game of Thrones)
 * April 12, 2014
 *)

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
                   (fun p -> ignore (new Town.town p !gold_id))

(* Generate Dany with kings_landing as a parameter *)
let gen_dany kings_landing =
  new Dany.dany (0,0) kings_landing

(* Generate the wall with kings_landing as a parameter *)
let gen_wall kings_landing =
  new Wall.wall (World.size-1,World.size-1) kings_landing

(* Generate a dragon with kings_landing and dany as parameters *)
let gen_dragon kings_landing dany =
  new Dragon.dragon (0,0) kings_landing dany

(* Generate a White Walker using kings_landing and the wall as parameters *)
let gen_white_walker kings_landing wall =
  new WhiteWalker.white_walker (World.size-1,World.size-1) kings_landing wall

let gen_city () =
  (* Don't ignore, since we will need to pass the city to some other objects. *)
  new KingsLanding.kings_landing (World.size/2,World.size/2)

(* Initializer functions *)
(* Part 1 initializer, which stores King's Landing, Dany, and the wall in local
 * variables to pass into other objects as parameters. *)
let part1_initializer () : unit =
  ignore (new Pond.pond (0,0));
  ignore (new Town.town (1,1) 0);
  let kings_landing = new KingsLanding.kings_landing (2,2) in
  ignore (new Human.human (3,3) (kings_landing :> WorldObjectI.world_object_i));
  let dany = new Dany.dany (4,4) kings_landing in
  ignore (new Dragon.dragon (5,5) kings_landing dany);
  let wall = new Wall.wall (6,6) kings_landing in
  ignore (new WhiteWalker.white_walker (7,7) kings_landing wall)

(* Part 2 initializer, which stores King's Landing in a local variable in order
 * to construct a human, dragon, and White Walker. *)
let part2_initializer () : unit =
  let kings_landing = gen_city () in
  let cast = (kings_landing :> WorldObjectI.world_object_i) in
  ignore (new Human.human (World.size/2+1,World.size/2) cast);
  (* Uses kings_landing as a dummy argument for the dragon's and white walker's
   * homes, because there isn't a Dany or a wall. *)
  ignore (gen_dragon kings_landing kings_landing);
  ignore (gen_white_walker kings_landing kings_landing)

(* Part 3 initializer, which stores King's Landing, Dany, and the wall in local
 * variables to pass into other objects as parameters. *)
let part3_initializer () : unit =
  let kings_landing = gen_city () in
  let cast = (kings_landing :> WorldObjectI.world_object_i) in
  let dany = gen_dany kings_landing in
  let wall = gen_wall kings_landing in
  ignore (gen_ponds ());
  ignore (gen_towns ());

  let count = ref 20 in
  while !count > 0 do
    ignore (new Human.human (World.size/2+1,World.size/2) cast);
    count := !count - 1
  done;

  ignore(gen_dragon kings_landing dany);
  ignore(gen_white_walker kings_landing wall)

(* Initializer for part 4 *)
let part4_initializer () : unit =
  ignore (gen_city ());
  ignore (gen_ponds ());
  ignore (gen_towns ())

(* Part 5 initializer, which stores King's Landing in a local variable in order
 * to construct Dany and the wall. *)
let final_initializer () : unit =
  let kings_landing = gen_city () in
  ignore (gen_dany kings_landing);
  ignore (gen_wall kings_landing);
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

(* Runs everything *)
let run () : unit =
  let initialize, part = parse_args () in
  UI.run_world initialize (event_loop part)
;;

run () ;;
