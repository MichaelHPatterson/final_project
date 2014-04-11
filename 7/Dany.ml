open Core.Std
open Event51
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_dragon_gold = 500

(** Dany will spawn a dragon when King's Lnading has collected a certain
    amount of gold. *)
class dany p city : world_object_i =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)
  initializer
    self#register_handler city#get_gold_event self#get_spawn

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private do_spawn (city_gold : int) : unit =
    (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is there no better way to do this? *)
    let no_dragons = World.fold (fun o b -> b && o#get_name <> "dragon") true in
    if city_gold >= spawn_dragon_gold && no_dragons then
      ignore(new Dragon.dragon self#get_pos city);
      print_string "dragons! \n";
      flush_all
      

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dany"

  method! draw = self#draw_circle Graphics.black (Graphics.rgb 0x80 0x00 0x80) "D"

  (* ### TODO: Part 6 Custom Events *)

end
