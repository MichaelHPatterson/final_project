open Core.Std
open Helpers
open Movable

(** Class type for objects which age over time.  After aging an ageable object
    will eventually die. *)
class type ageable_t =
object
  inherit movable_t

  (** Draw just the picture portion of this object. After drawing this
      picture, the life bar for the object will be drawn on top. *)
  method draw_picture : unit

  (** Reset the life of this object to be the maximum amount. *)
  method reset_life : unit

end

class ageable p inv_speed starting_lifetime max_lifetime : ageable_t =
object (self)
  inherit movable p inv_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)
  
  (* sets initial lifetime equal to starting_lifetime argument *)
  val mutable lifetime = starting_lifetime

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging ### *)

  (* adds the lifetime listener to age event *)
  initializer
    self#register_handler World.age_event self#do_age

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)

  (* function that either decrements life or calls die element *)
  method private do_age () : unit =
    if lifetime > 0 then lifetime <- lifetime - 1;
    if lifetime = 0 then self#die

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* draws health bar of a human *)
  method private draw_life : unit =
    let v = Float.of_int lifetime /. Float.of_int max_lifetime in
    self#draw_status_bar Graphics.red v

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* calls both draw_picture and draw_life methods, drawing the object itself
   * and its corresponding health bar *)
  method! draw : unit =
    self#draw_picture ;
    self#draw_life

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

  (* Default draw_picture function -- draws a green circle with no text *)
  method draw_picture : unit = self#draw_circle Graphics.green Graphics.black ""

  (* Restores life to maximum value *)
  method reset_life = lifetime <- max_lifetime
end
