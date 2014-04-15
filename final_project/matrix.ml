exception TODO

(* Note: this is just preliminary and could require some modifications. *)
module FloatMatrix =
struct
  type vec = float array
  type mat = vec array

  exception SizeMismatch
  exception IndexOutOfBounds

  let map2 ~(f : float -> float -> 'a) (l1 : vec) (l2 : vec) : 'a array =
    let len1 = Array.length l1 in
    if len1 <> Array.length l2 then raise SizeMismatch
    else
      let result = Array.create ~len:len1 (f l1.(0) l2.(0)) in
      for i = 0 to len1 - 1 do
        result.(i) <- f l1.(i) l2.(i)
      done;
      result

  let add_vec : vec -> vec -> vec = map2 ~f:(+.)

  let scalar_mult (factor : float) (m : vec) = Array.map ~f:(( *. ) factor) m

  (* Multiplies a square matrix m with a vector v. Interprets each sub-array in
   * m as a column of m. *)
  let mult (m : mat) (v : vec) : vec =
    let len = Array.length m in
    if len <> Array.length v then raise SizeMismatch
    else
      let result : vec ref = ref (Array.create ~len 0.) in
      for i = 0 to len - 1 do
        result := add_vec !result (scalar_mult v.(i) m.(i))
      done;
      !result

  (* Constructs the nth basis vector (zero-indexed) in R^dim. *)
  let basis_vec ~(dim : int) (n : int) : vec =
    if n >= dim then raise IndexOutOfBounds
    else
      let result : vec = Array.create ~len:dim 0. in
      result.(n) <- 1.;
      result

  let identity (n : int) : mat =
    Array.init n ~f:(fun i -> basis_vec ~dim:n i)

  let transpose (m : mat) : mat =
    let num_cols = Array.length m in
    if num_cols = 0 then m
    else
      let num_rows = Array.length m.(1) in
      let result = Array.make_matrix num_rows num_cols 0. in
      for i = 0 to num_rows - 1 do
        let v : vec = Array.create ~len:num_cols 0. in
        for j = 0 to num_cols - 1 do
	  v.(j) <- m.(j).(i)
        done;
        result.(i) <- v
      done;
      result

  let swap (m : mat) (n1 : int) (n2 : int) : unit =
    let len = Array.length m in
    if n1 >= len || n2 >= len then raise IndexOutOfBounds
    else if n1 = n2 then ()
    else
      let v = m.(n1) in
      m.(n1) <- m.(n2);
      m.(n2) <- v

  (* Note: This doesn't work at all *)
  let row_reduce (m : mat) : mat =
    (* Column reduces rows n through Array.length m - 1 of m. Assumes that <piv>
     * pivotal 1's have been generated so far. *)
    let rec col_reduce_past (m : mat) (n : int) (piv : int) : mat =
      let len = Array.length m in
      if n >= len || piv >= len then m else
      (* (a) SWAP rows so that the leftmost column that is not all zeroes has a
             nonzero entry in the first row. *)
      (* Returns the column index of the first nonzero element in the specified
       * row <row>, ignoring the first <piv> elements. *)
      let first_nonzero (m : mat) (row : int) : (int * float) option =
	let len = Array.length m in
        let ans = ref (len, 0.) in
        for i = len - 1 downto piv do
	  if m.(i).(row) <> 0. then ans := (i, m.(i).(row))
	done;
        !ans
      in
      match first_nonzero m n with
      | None ->
        (* No pivotal 1 found in that row. *)
        col_reduce_past m (n + 1) piv
      | Some (swap_col, value) ->
        (* Pivotal value of <value> found in <swap_col>th position. *)
	swap m n swap_col;

        (* (b) DIVIDE by this entry to get a pivotal 1. *)
        m.(n) <- scalar_mult (1. /. value) m.(n);

        (* (c) SUBTRACT multiples of the first row from the others to clear out the
            rest of the column under the pivotal 1. *)
        for i = 0 to len - 1 do
	  if i <> n then m.(i) <- add_vec m.(i) (scalar_mult (-1. *. m.(i).(n)) m.(n))
	done;

	col_reduce_past m (n + 1) (piv + 1)
    transpose (col_reduce_past (transpose m) 0 0)

  (* Note: This is totally wrong *)
  (*
  let row_reduce (m : mat) : mat =
    let m = transpose m in
    let len = Array.length m in
    let tracker : bool array = Array.create ~len false in
    let next_col (start_from : int) : col_remaining =
      if start_from >= Array.length tracker then Done
      else if tracker.(start_from) then start_from
      else next_col (start_from + 1)
    in
    for i = 0 to len - 1 do
      next_col i
    done;
    (* Tries to put a pivotal 1 at j,k. *)
    let jk_col_reduce (n : mat) (j : int) (k : int) : mat =
      let len = Array.length n in
      if j >= len or k >= len then raise IndexOutOfBounds
      else if n.(j).(k) = 0. then n
      else
        let _ = n.(j) <- scalar_mult (1. /. n.(j).(k)) n.(j) in
        for l = 0 to len - 1 do
          if j <> l then n.(l) <- add_vec n.(l) (scalar_mult (-1.) (scalar_mult n.(j).(k) n.(j)))
        done;
        tracker.(j) <- true;
        n
    in
    for i = 0 to Array.length m - 1 do
      jk_col_reduce m i
    done;
    transpose m
   *)
end

(* For typing convenience *)
module M = FloatMatrix
