(* PS4
 * CS51 Spring 2014
 * Authors: Madhu Vijay & Michael Patterson
 * Module warmup exercises.
 *)

(*****************************************************************************)
(*                           Part 1 - Warmup                                 *)
(*****************************************************************************)

open Core.Std

(*>* Problem 1.0 *>*)

(* Module signature for some mathematical values and functions. *)
module type MATH =
sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
end

(* Module implementing the MATH signature above. *)
module Math : MATH =
  struct
    (* Uses built-in functions and values for pi, cos, sin, and sum. *)
    let pi = 4.0 *. atan 1.0
    let cos = cos
    let sin = sin
    let sum = (+.)
    let max lst = 
      let rec max_helper = function
        | [x] -> x
        | x :: xs' -> max x (max_helper xs')
        | [] -> failwith "empty list" in
      match lst with
      | [] -> None
      | _ -> Some (max_helper lst)
  end


(*>* Problem 1.1 *>*)

(* Simple signature for lists. *)
module type LIST =
sig
 (* If I write "type 'a t" I get type errors. The following line works
  * perfectly but seems wrong compared to section notes. However it seems
  * necessary because we can't put it in the module itself. *)
  type 'a t = 'a list
  val length : 'a t -> int
  val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init: 'b -> 'b
  val rev : 'a t -> 'a t
end

module MyList = (List : LIST);;

let _ =
    assert(MyList.length [1;2;3] = 3);
    assert(MyList.fold_right ~f:(+) ~init:0 [1;2;3] = 6);
    assert(MyList.rev [1;2;3] = [3;2;1])




(*>* Problem 1.2 *>*)

(* Module for Rob *)
module Rob =
struct
    type house =
        Adams | Lowell | Quincy |
        Kirkland | Winthrop | Eliot |
        Leverett | Mather | Dunster |
        Pforzheimer | Cabot | Currier

    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : house
    }

    let hometown = "Franklin Borough, NJ"
    let year = 2013
    let concentration = "Computer Science"
    let house = Kirkland
    let fold = List.fold_left ~f:(+)

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let grade_assignment assignment =
      "Everyone gets a perfect score for pset " ^ String.strip assignment ^ "!"

    let favorite_function _ = failwith "I don't have a favorite function"
    let least_favorite_function = ( ** )

    let print_info () =
        let _ = print_string (
            info.hometown ^ "\n" ^
            string_of_int year ^ "\n" ^
            info.concentration ^ "\n") in
        match info.house with
        | Kirkland -> print_string "Kirkland!\n"
        | _ -> failwith "Do any other houses matter?"

end

(* Module for Ben *)
module Ben =
struct
    type info = {
        hometown : string;
        house : string;
        year : int;
        concentration : string
    }

    let least_favorite_function = (land)

    let rec info = {
        hometown = hometown;
        house = house;
        year = 2015;
        concentration = "Computer Science"
    }
    and hometown = "Holland, Michigan"
    and house = "Dunster"

    let grade_assignment assignment =
      "Everyone gets a zero for pset " ^ string_of_int assignment ^ ">:-("

    let favorite_function x y =
        log x +. log y +. Int64.to_float 0x46524F535459L

    let print_info = fun _ ->
        print_string (info.house ^ "\n" ^ info.hometown ^ "\n" ^
            string_of_int info.year ^ "\n" ^ info.concentration)

    let f = (-)
    let fold = List.fold_right ~f

end

(* TF signature that exposes as much of "*both" Ben and Rob as possible. *)

module type TF =
sig
  type info
  val info : info
  val hometown : string
  val fold : int list -> init:int -> int
end

module TFBen = (Ben : TF)
module TFRob = (Rob : TF)

(*>* Problem 1.3 *>*)
