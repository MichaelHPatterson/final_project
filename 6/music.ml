(* Partner 1's name: Madhusudan ("Madhu") Vijay
 * Partner 1's code.seas account: mvijay
 *
 * Partner 2's name: Michael Patterson
 * Partner 2's code.seas account: michaelpatterson
 *
 * CS51, Problem Set 6
 * March 28, 2014
 *
 * Part 3: using streams to represent music
 *)

open Core.Std

exception InvalidHex
exception InvalidPitch

(***** Type definitions *****)
type p = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
type pitch = p * int

type event = Tone of float * pitch * int | Stop of float * pitch

type obj = Note of pitch * float * int | Rest of float

(* Converts a p to the corresponding integer *)
let p_to_int p =
  match p with | C -> 0 | Db -> 1 | D -> 2 | Eb -> 3 | E -> 4 | F -> 5
    | Gb -> 6 | G -> 7 | Ab -> 8 | A -> 9 | Bb -> 10 | B -> 11

(* Converts an integer to a p *)
let int_to_p n =
  if (n < 0) || (n > 11) then raise InvalidPitch else
    let pitches = [C;Db;D;Eb;E;F;Gb;G;Ab;A;Bb;B] in
  List.nth_exn pitches n

(***** Streams Code *****)
(* Type definition for a stream *)
type 'a stream = unit -> 'a str
and 'a str = Cons of 'a * 'a stream ;;

(* Returns the head of s *)
let head (s: 'a stream) : 'a =
  let Cons(v, _) = s () in v
;;

(* Returns the tail of s *)
let tail (s: 'a stream) : 'a stream =
  let Cons(_, s') = s () in s'
;;

(* Maps a function f over the stream s *)
let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  fun () -> Cons(f (head s), map f (tail s))
;;


(***** MIDI Output Code *****)
let hex_to_int hex = int_of_string ("0x"^hex)

let int_to_hex n = Printf.sprintf "%02x" n

let rec output_hex outchan hex =
  let len = String.length hex in
  if (len = 0) then ()
  else (if (len < 2) then raise InvalidHex
  else (output_byte outchan
    (hex_to_int (String.sub hex ~pos:0 ~len:2)));
        (output_hex outchan (String.sub hex ~pos:2 ~len:(len - 2))))

let ticks_per_q = 32

let header = "4D546864000000060001000100"^(int_to_hex ticks_per_q)^"4D54726B"
let footer = "00FF2F00"

let pitch_to_hex pitch =
  let (p, oct) = pitch in int_to_hex ((oct+1)*12+(p_to_int p))

let time_to_hex time =
  let measure = ticks_per_q * 4 in
  let itime = Float.to_int (time *. (float measure)) in
  if itime < measure then (int_to_hex itime)
  else "8"^(string_of_int (itime / measure))^
    (Printf.sprintf "%02x" (itime mod measure))

let rec insts playing pitch =
  match playing with
    | [] -> (0, [])
    | (pitch2, n)::t -> if pitch2 = pitch then (n, playing) else
        let (n2, p2) = insts t pitch in (n2, (pitch2, n)::p2)

let shift (by : float) (e : event) =
  match e with
    | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
    | Stop (time, pit) -> Stop (time +. by, pit)

let shift_start (by : float) (str : event stream) =
  let Cons (e, t) = str () in
    fun () -> Cons(shift by e, t)

let stream_to_hex (n : int) (str : event stream) =
  let rec sthr n str playing =
  if n = 0 then "" else
  match str () with
    | Cons(Tone (t, pitch, vol), tl) ->
        let (i, np) = insts playing pitch in
          (time_to_hex t)^"90"^(pitch_to_hex pitch)^(int_to_hex vol)^
            (sthr (n-1) tl ((pitch, i+1)::np))
    | Cons(Stop (t, pitch), tl) ->
        let (i, np) = insts playing pitch in
          if i>1 then sthr (n-1) (shift_start t tl) ((pitch, i-1)::np)
          else (time_to_hex t)^(pitch_to_hex pitch)^"00"^
          (sthr (n-1) tl ((pitch, i-1)::np))
  in sthr n str []

let output_midi filename n str =
  let hex = stream_to_hex n str in
  let outchan = open_out_bin filename in
  output_hex outchan header;
  output_binary_int outchan ((String.length hex) / 2 + 4);
  output_hex outchan hex;
  output_hex outchan footer;
  flush outchan;
  Out_channel.close outchan

(******* Music code ********)
(* Shifts the time of an event by "by". *)
let shift (by : float) (e : event) =
  match e with
    | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
    | Stop (time, pit) -> Stop (time +. by, pit)

(* Shifts the start of an event stream by "by". *)
let shift_start (by : float) (str : event stream) =
  let Cons (e, t) = str () in
    fun () -> Cons(shift by e, t)


(*>* Problem 3.1 *>*)

(* Builds a music stream from a finite list of musical objects. The resulting
 * stream repeats the music forever. Uses a recursive helper function. *)
let rec list_to_stream (lst : obj list) : event stream =
  (* float 'time' is used to pass duration of previous object, for use in next
   * event. when time is 0.0, event is simultaneous to last event *)
  let rec list_to_stream_rec (time : float) (nlst : obj list) : event stream =
    match nlst with
    | [] -> list_to_stream lst
    | Note (p, dur, vol) :: rem ->
      fun () -> Cons (Tone (time, p, vol), fun () -> Cons (Stop (dur, p), 
                      list_to_stream_rec 0.0 rem))
    | Rest dur' :: rem -> list_to_stream_rec (time +. dur') rem in 
  list_to_stream_rec 0.0 lst

(* Helper function that returns the time of an event. *)
let time_of_event (e : event) : float =
  match e with
    | Tone (time, _, _) -> time
    | Stop (time, _) -> time


(*>* Problem 3.2 *>*)

(* Merges two event streams. Events that happen earlier in time appear earlier
 * in the merged stream. *)
let rec pair (a : event stream) (b : event stream) : event stream =
  (* Helper function that modifies an event stream's head by shifting it
   * backward by "time_of_event evt". Returns stream with modified head. *)
  let head_mod (evt : event) = shift_start (-. time_of_event evt) in
  (* if time head A < time head B, then head A is next elt in stream, and vice 
   * versa. if =, then events are simultaneous, and B is picked arbitrarily. *)
  if time_of_event (head a) < time_of_event (head b) then
    (fun () -> Cons (head a, pair (tail a) (head_mod (head a) b)))
  else (fun () -> Cons (head b, pair (head_mod (head b) a) (tail b)))
  

(*>* Problem 3.3 *>*)

(* Function from the distribution code -- moves the pitch (p, oct) up by
 * half_steps pitches. *)
let transpose_pitch (p, oct) half_steps =
  let newp = (p_to_int p) + half_steps in
    if newp < 0 then
      if (newp mod 12) = 0 then (C, oct + (newp / 12))
      else (int_to_p ((newp mod 12) + 12), oct - 1 + (newp / 12))
    else (int_to_p (newp mod 12), oct + (newp / 12))

(* Takes an event stream and moves each pitch up by half_steps pitches, with
 * the help of the transpose_pitch function. *)
let rec transpose (str : event stream) (half_steps : int) : event stream =
  match head str with
  | Tone (time, p, vol) ->
    let p' = transpose_pitch p half_steps in
    fun () -> Cons (Tone (time, p', vol), transpose (tail str) half_steps)
  | Stop (time, p) -> 
    let p' = transpose_pitch p half_steps in
    fun () -> Cons (Stop (time, p'), transpose (tail str) half_steps)
    

(* Some functions for convenience. *)
let quarter pt = Note(pt,0.25,60);;

let eighth pt = Note(pt,0.125,60);;


(* Creates some scales and outputs them in scale.mid, for testing. *)
let scale1 = list_to_stream (List.map ~f:quarter [(C,3);(D,3);(E,3);(F,3);(G,3);
                                            (A,3);(B,3);(C,4)]);;

let scale2 = transpose scale1 7;;

let scales = pair scale1 scale2;;

output_midi "scale.mid" 32 scales;;


(*>* Problem 3.4 *>*)

(* Creates the "bass" and "melody" streams, which are used to construct
 * Pachelbel's canon. *)

let bass = list_to_stream (List.map ~f:quarter [(D,3);(A,2);(B,2);(Gb,2);(G,2);
                                             (D,2);(G,2);(A,2)]);;

let slow = [(Gb,4);(E,4);(D,4);(Db,4);(B,3);(A,3);(B,3);(Db,4);(D,4);
            (Db,4);(B,3);(A,3);(G,3);(Gb,3);(G,3);(E,3)];;
let fast = [(D,3);(Gb,3);(A,3);(G,3);(Gb,3);(D,3);(Gb,3);(E,3);(D,3);(B,2);
            (D,3);(A,3);(G,3);(B,3);(A,3);(G,3)];;

let melody = list_to_stream ((List.map ~f:quarter slow) @
                (List.map ~f:eighth fast));;


(* Defines a stream for Pachelbel's canon using the bass and melody streams
 * and the appropriate time offsets (2, 4, and 6) for melody. *)
let canon =
  (* Small helper that shifts the melody stream by "by" measures. *)
  let shift_melody (by : float) = shift_start by melody in
  pair (pair (pair bass (shift_melody 2.)) (shift_melody 4.)) (shift_melody 6.)
;;

output_midi "canon.mid" 176 canon;;


(* Some other music event streams from distribution code, for testing. *)

let part1 = list_to_stream [Rest 0.5; Note((D,4),0.75,60); Note((E,4),0.375,60);
                            Note((D,4),0.125,60); Note((B,3),0.25,60);
                            Note((Gb,3),0.1875,60); Note((G,3),0.0625,60)];;

let part2 = list_to_stream [Note((G,3),0.1875,60); Note((A,3),0.0625,60);
                            Note((B,3),0.375,60); Note((A,3),0.1875,60);
                            Note((B,3),0.0625,60); Note((C,4),0.5,60);
                            Note((B,3),0.5, 60)];;

let part3 = list_to_stream [Note((G,3),1.,60); Note((G,3),0.5,60);
                            Note((E,3),0.1875,60);
                            Note((Gb,3),0.0625,60); Note((G,3),0.25, 60);
                            Note((E,3),0.25,60)];;

let part4 = list_to_stream [Rest(0.25); Note((G,3),0.25,60);
                            Note((Gb,3),0.25,60); Note((E,3),0.375,60);
                            Note((D,3),0.125,60); Note((C,3),0.125,60);
                            Note((B,2),0.125,60); Note((A,2),0.25,60);
                            Note((E,3),0.375,60); Note((D,3),0.125,60)];;

(* Combines part1 through part4 above into a single stream, and outputs the
 * result in the file test.mid. *)
output_midi "test.mid" 176 (pair part1 (pair part2 (pair part3 part4)));;

(*>* Problem 3.5 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 150
