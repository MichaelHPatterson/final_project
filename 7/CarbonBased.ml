open Core.Std
open Ageable

(** Carbon based objects eventually die, and leave dust behind when they do. *)
class carbon_based p inv_speed starting_lifetime max_lifetime : ageable_t =
object (self)
  inherit ageable p inv_speed starting_lifetime max_lifetime

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging *)

  initializer
    (* unit -> unit function that spawns a new dust object upon death *)
    let spawn_dust () = ignore(new Dust.dust self#get_pos self#get_name) in
    self#register_handler self#get_die_event spawn_dust

end
