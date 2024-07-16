(*  TODO: Use one big array *)
(*  TODO: Add secondary colors *)

let debug = false
let ( += ) a b = a := !a + b
let ( -= ) a b = a := !a - b
let size_of_col ec col = 2 + (2 * List.length (Ec.get_rows_of_col ec col))

let size_of_row ec row =
  2
  + (2 * List.length (Ec.get_primary_cols_of_row ec row))
  (* A given row must ALWAYS be part of its rows_of_row list *)
  + (2 * (List.length (Ec.get_rows_of_row ec row) - 1))

let problem_size ec =
  let total_size = ref 2 in
  total_size += (2 * (Ec.get_primary_cols ec |> Seq.length));
  Seq.iter
    (fun col -> total_size += size_of_col ec col)
    (Ec.get_primary_cols ec);
  Seq.iter (fun row -> total_size += size_of_row ec row) (Ec.get_rows ec);
  !total_size

let make ec =
  let pb = Array.make (problem_size ec) 0 in
  let col_addresses = Hashtbl.create 10 and row_addresses = Hashtbl.create 10 in
  let ptr = ref (2 + (2 * (Ec.get_primary_cols ec |> Seq.length))) in
  (* Compute primary column positions *)
  Seq.iter
    (fun col ->
      Hashtbl.add col_addresses col (!ptr + 1);
      ptr += size_of_col ec col)
    (Ec.get_primary_cols ec);
  (* Compute row positions *)
  Seq.iter
    (fun row ->
      Hashtbl.add row_addresses row
        (!ptr + 1 + (2 * List.length (Ec.get_primary_cols_of_row ec row)));
      ptr += size_of_row ec row)
    (Ec.get_rows ec);
  if debug then Printf.printf "!ptr = %d\n" !ptr;
  (* assert (!ptr = Array.length pb); *)
  (* Fill primary colList *)
  pb.(0) <- 2 (* head *);
  let tail = ref 2 in
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      pb.(!tail) <- col_addr;
      pb.(col_addr - 1) <- !tail;
      tail += 2)
    (Ec.get_primary_cols ec);
  pb.(1) <- !tail;
  (* Fill cols_of_rows and rows_of_cols *)
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      pb.(col_addr) <- col_addr + 1)
    (Ec.get_primary_cols ec);
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      pb.(row_addr - 1) <- row_addr - 3)
    (Ec.get_rows ec);
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      List.iter
        (fun row ->
          let row_addr = Hashtbl.find row_addresses row in
          let col_pos = pb.(col_addr) and row_pos = pb.(row_addr - 1) in
          pb.(col_pos) <- row_pos;
          pb.(col_pos + 1) <- col_addr;
          pb.(row_pos) <- col_pos;
          pb.(row_pos + 1) <- row_addr;
          pb.(col_addr) <- col_pos + 2;
          pb.(row_addr - 1) <- row_pos - 2)
        (Ec.get_rows_of_col ec col))
    (Ec.get_primary_cols ec);
  (* Fill rows_of_rows *)
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      pb.(row_addr) <- row_addr + 1)
    (Ec.get_rows ec);
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      List.iter
        (fun row' ->
          if row' < row then (
            let row'_addr = Hashtbl.find row_addresses row' in
            let row_pos = pb.(row_addr) and row'_pos = pb.(row'_addr) in
            pb.(row_pos) <- row'_pos;
            pb.(row_pos + 1) <- row_addr;
            pb.(row'_pos) <- row_pos;
            pb.(row'_pos + 1) <- row'_addr;
            pb.(row_addr) <- row_pos + 2;
            pb.(row'_addr) <- row'_pos + 2))
        (Ec.get_rows_of_row ec row))
    (Ec.get_rows ec);
  pb

(* Total size : 2 + 2 * 6 + 2 * (15 - 3) *)

(*
00: head: 2; tail: 6 ;
02:  7;  0; Col 2
04: 13;  0; Col 1
C2:     07  R1      R3
06:  2; 12; 32; 35; 24; 27;
C1      13  R1      R2
12:  4; 18; 30; 35; 18; 21;
R2  C1          21  R1
18: 16; 13; 16; 24; 36; 35;
R3  C2          27  R1
24: 10;  7; 22; 30; 38; 35; 
R1                      35
30: 14; 13;  8;  7; 28; 40; 22; 21; 28; 27;
...
40:
*)

let hide_cr pb cr_pos =
  let rc_pos = pb.(cr_pos) in
  let row = pb.(rc_pos + 1) in
  assert (pb.(rc_pos) = cr_pos);
  let rc_end = pb.(row - 1) + 2 in
  pb.(row - 1) <- rc_end;
  if rc_pos > rc_end then (
    let cr_pos' = pb.(rc_end) in
    assert (pb.(cr_pos') = rc_end);
    pb.(rc_pos) <- cr_pos';
    pb.(rc_end) <- cr_pos;
    pb.(cr_pos) <- rc_end;
    pb.(cr_pos') <- rc_pos)

let unhide_cr pb cr_pos =
  let rc_pos = pb.(cr_pos) in
  let row = pb.(rc_pos + 1) in
  pb.(row - 1) <- pb.(row - 1) - 2

let hide_rc pb rc_pos =
  let cr_pos = pb.(rc_pos) in
  let col = pb.(cr_pos + 1) in
  assert (pb.(cr_pos) = rc_pos);
  let cr_end = pb.(col) - 2 in
  pb.(col) <- cr_end;
  if cr_pos < cr_end then (
    let rc_pos' = pb.(cr_end) in
    assert (pb.(rc_pos') = cr_end);
    pb.(cr_pos) <- rc_pos';
    pb.(cr_end) <- rc_pos;
    pb.(rc_pos) <- cr_end;
    pb.(rc_pos') <- cr_pos)

let unhide_rc pb rc_pos =
  let cr_pos = pb.(rc_pos) in
  let col = pb.(cr_pos + 1) in
  pb.(col) <- pb.(col) + 2

let hide_rr pb rr_pos =
  let rr_sop = pb.(rr_pos) in
  let wor = pb.(rr_sop + 1) in
  assert (pb.(rr_sop) = rr_pos);
  let rr_dne = pb.(wor) - 2 in
  pb.(wor) <- rr_dne;
  if rr_sop < rr_dne then (
    let rr_pos' = pb.(rr_dne) in
    assert (pb.(rr_pos') = rr_dne);
    pb.(rr_sop) <- rr_pos';
    pb.(rr_dne) <- rr_pos;
    pb.(rr_pos) <- rr_dne;
    pb.(rr_pos') <- rr_sop)

let unhide_rr pb rr_pos =
  let rr_sop = pb.(rr_pos) in
  let wor = pb.(rr_sop + 1) in
  pb.(wor) <- pb.(wor) + 2

let disable_col pb col =
  let cr_pos = ref (col + 1) and cr_end = pb.(col) in
  while !cr_pos < cr_end do
    hide_cr pb !cr_pos;
    cr_pos += 2
  done

let enable_col pb col =
  let cr_pos = ref (pb.(col) - 2) in
  while !cr_pos > col do
    unhide_cr pb !cr_pos;
    cr_pos -= 2
  done

let disable_row pb row =
  let rc_pos = ref (row - 3) and rc_end = pb.(row - 1) in
  while !rc_pos > rc_end do
    hide_rc pb !rc_pos;
    rc_pos -= 2
  done;
  let rr_pos = ref (row + 1) and rr_end = pb.(row) in
  while !rr_pos < rr_end do
    hide_rr pb !rr_pos;
    rr_pos += 2
  done

let enable_row pb row =
  let rc_pos = ref (pb.(row - 1) + 2) in
  while !rc_pos < row - 1 do
    unhide_rc pb !rc_pos;
    rc_pos += 2
  done;
  let rr_pos = ref (pb.(row) - 2) in
  while !rr_pos > row do
    unhide_rr pb !rr_pos;
    rr_pos -= 2
  done

let move_to_head pb col col_pos =
  let head = pb.(0) in
  if col_pos > head then (
    let col' = pb.(head) in
    pb.(head) <- col;
    pb.(col - 1) <- head;
    pb.(col_pos) <- col';
    pb.(col' - 1) <- col_pos);
  pb.(0) <- head + 2

let remove_from_head pb = pb.(0) <- pb.(0) - 2

let move_to_tail pb col col_pos =
  let tail = pb.(1) - 2 in
  pb.(1) <- tail;
  if col_pos < tail then (
    let col' = pb.(tail) in
    pb.(tail) <- col;
    pb.(col - 1) <- tail;
    pb.(col_pos) <- col';
    pb.(col' - 1) <- col_pos)

let remove_from_tail pb = pb.(1) <- pb.(1) + 2

let select_row pb row =
  disable_row pb row;
  let rc_pos = ref (row - 3) and rc_end = pb.(row - 1) in
  while !rc_pos > rc_end do
    let cr_pos = pb.(!rc_pos) in
    let col = pb.(cr_pos + 1) in
    let col_pos = pb.(col - 1) in
    move_to_tail pb col col_pos;
    rc_pos -= 2
  done;
  let rr_pos = ref (row + 1) and row_end = pb.(row) in
  while !rr_pos < row_end do
    let rr_sop = pb.(!rr_pos) in
    let wor = pb.(rr_sop + 1) in
    disable_row pb wor;
    rr_pos += 2
  done

let deselect_row pb row =
  let rr_pos = ref (pb.(row) - 2) in
  while !rr_pos > row do
    let rr_sop = pb.(!rr_pos) in
    let wor = pb.(rr_sop + 1) in
    enable_row pb wor;
    rr_pos -= 2
  done;
  let rc_pos = ref (pb.(row - 1) + 2) in
  while !rc_pos < row - 1 do
    remove_from_tail pb;
    rc_pos += 2
  done;
  enable_row pb row

let cover pb col =
  disable_col pb col;
  move_to_head pb col pb.(col - 1)

let uncover pb col =
  remove_from_head pb;
  enable_col pb col

let rec move_in pb =
  let head = pb.(0) and tail = pb.(1) in
  if pb.(0) < pb.(1) then
    let rec aux cnd_pos cnd_col cnd_card cur_pos =
      if cur_pos >= tail then (
        cover pb cnd_col;
        pb.(head + 1) <- cnd_col + 1;
        move_on pb)
      else
        let cur_col = pb.(cur_pos) in
        let cur_card = pb.(cur_col) - cur_col in
        if cur_card < cnd_card then aux cur_pos cur_col cur_card (cur_pos + 2)
        else aux cnd_pos cnd_col cnd_card (cur_pos + 2)
    in
    let col = pb.(head) in
    aux head col (pb.(col) - col) (head + 2)

and move_on pb =
  let col = pb.(pb.(0) - 2) and cr_pos = pb.(pb.(0) - 1) in
  if cr_pos >= pb.(col) then (
    uncover pb col;
    move_out pb)
  else
    let rc_pos = pb.(cr_pos) in
    let row = pb.(rc_pos + 1) in
    select_row pb row;
    move_in pb

and move_out pb =
  let head = pb.(0) in
  if head > 2 then (
    let cr_pos = pb.(head - 1) in
    let rc_pos = pb.(cr_pos) in
    let row = pb.(rc_pos + 1) in
    deselect_row pb row;
    pb.(head - 1) <- cr_pos + 2;
    move_on pb)

let count_solutions pb =
  let cnt = ref 0 in
  move_in pb;
  while pb.(0) > 2 do
    incr cnt;
    move_out pb
  done;
  !cnt
