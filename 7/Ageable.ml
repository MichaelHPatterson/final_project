open Core.Std
open Helpers

(** Class type for objects which age over time.  After aging an ageable object
    will eventually die. *)
class type ageable_t =
object
  inherit Movable.movable_t

  (** Draw just the picture portion of this object. After drawing this
      picture, the life bar for the object will be drawn on top. *)
  method draw_picture : unit

  (** Reset the life of this object to be the maximum amount. *)
  method reset_life : unit

end

class ageable p inv_speed starting_lifetime max_lifetime : ageable_t =
object (self)
  inherit Movable.movable p inv_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable lifetime = starting_lifetime

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging ### *)
  initializer
    self#register_handler World.age_event self#do_age

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  (* Note: Is the location of this function in the file correct? In Human.ml, we have do_action under the "WorldObjectI Methods"; should we have do_age under the same header in this file as well? If we don't, then we wouldn't be consistent; but if we do, then this header would be empty. Also, do_age (and do_action) are event handlers, so this seems like it might belong under the "Event Handlers section". *)
  method private do_age () : unit =
    if lifetime > 0 then lifetime <- lifetime - 1;
    if lifetime = 0 then self#die

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private draw_life : unit =
    let v = Float.of_int lifetime /. Float.of_int max_lifetime in
    self#draw_status_bar Graphics.red v

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method! draw : unit =
    self#draw_picture ;
    self#draw_life

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)
  (* Default draw_picture function -- draws a white circle with no text *)
  method draw_picture : unit = self#draw_circle Graphics.white Graphics.black ""
  method reset_life = lifetime <- max_lifetime
end
