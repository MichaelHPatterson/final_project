open Core.Std
(* ### Part 4 Aging ### *)
let dust_lifetime = 50

(** Dust is what remains when carbon-based objects die. *)
class dust p (name:string) : Ageable.ageable_t =
object (self)
  inherit Ageable.ageable p None dust_lifetime dust_lifetime

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

  method! draw_picture : unit =
    let name_sub : string = String.sub name ~pos:0 ~len:2 in
    self#draw_circle (Graphics.rgb 150 150 150) Graphics.black name_sub
end
