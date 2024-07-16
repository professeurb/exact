(*  TODO: Use one big array *)
(*  TODO: Add secondary colors *)

type ('a, 'b) col = {
  col_name : 'a;
  mutable cr_end : int;
  mutable cr_arr : (('a, 'b) row * int) array;
  problem : ('a, 'b) t;
  mutable pb_pos : int;
}

and ('a, 'b) row = {
  row_name : 'b;
  mutable rc_end : int;
  mutable rc_arr : (('a, 'b) col * int) array;
  mutable rr_end : int;
  mutable rr_arr : (('a, 'b) row * int) array;
}

and ('a, 'b) t = {
  mutable head : int;
  mutable tail : int;
  mutable cols : (('a, 'b) col * int ref) array;
  mutable rows : ('a, 'b) row array;
}

let hide_rc row rc_pos =
  let col, cr_pos = row.rc_arr.(rc_pos) in
  assert (fst col.cr_arr.(cr_pos) == row);
  assert (snd col.cr_arr.(cr_pos) = rc_pos);
  let cr_end = col.cr_end - 1 in
  col.cr_end <- cr_end;
  if cr_pos < cr_end then (
    let row', c_pos' = col.cr_arr.(cr_end) in
    col.cr_arr.(cr_pos) <- (row', c_pos');
    row'.rc_arr.(c_pos') <- (col, cr_pos);
    col.cr_arr.(cr_end) <- (row, rc_pos);
    row.rc_arr.(rc_pos) <- (col, cr_end))

let unhide_rc row c_pos =
  let col, _ = row.rc_arr.(c_pos) in
  col.cr_end <- col.cr_end + 1

let hide_cr col cr_pos =
  let row, rc_pos = col.cr_arr.(cr_pos) in
  assert (fst row.rc_arr.(rc_pos) == col);
  assert (snd row.rc_arr.(rc_pos) = cr_pos);
  let rc_end = row.rc_end - 1 in
  row.rc_end <- rc_end;
  if rc_pos < rc_end then (
    let col', cr_pos' = row.rc_arr.(rc_end) in
    row.rc_arr.(rc_pos) <- (col', cr_pos');
    col'.cr_arr.(cr_pos') <- (row, rc_pos);
    row.rc_arr.(rc_end) <- (col, cr_pos);
    col.cr_arr.(cr_pos) <- (row, rc_end))

let unhide_cr col r_pos =
  let row, _ = col.cr_arr.(r_pos) in
  row.rc_end <- row.rc_end + 1

let hide_rr row w_pos =
  let wor, r_pos = row.rr_arr.(w_pos) in
  assert (fst wor.rr_arr.(r_pos) == row);
  assert (snd wor.rr_arr.(r_pos) = w_pos);
  let rr_end = wor.rr_end - 1 in
  wor.rr_end <- rr_end;
  if r_pos < rr_end then (
    let row', r_pos' = wor.rr_arr.(rr_end) in
    wor.rr_arr.(r_pos) <- (row', r_pos');
    row'.rr_arr.(r_pos') <- (wor, r_pos);
    wor.rr_arr.(rr_end) <- (row, w_pos);
    row.rr_arr.(w_pos) <- (wor, rr_end))

let unhide_rr row w_pos =
  let wor, _ = row.rr_arr.(w_pos) in
  wor.rr_end <- wor.rr_end + 1

let disable_col col =
  for row_pos = 0 to col.cr_end - 1 do
    hide_cr col row_pos
  done

let enable_col col =
  for row_pos = col.cr_end - 1 downto 0 do
    unhide_cr col row_pos
  done

let disable_row row =
  for col_pos = 0 to row.rc_end - 1 do
    hide_rc row col_pos
  done;
  for row_pos = 0 to row.rr_end - 1 do
    hide_rr row row_pos
  done

let enable_row row =
  for row_pos = row.rr_end - 1 downto 0 do
    unhide_rr row row_pos
  done;
  for col_pos = row.rc_end - 1 downto 0 do
    unhide_rc row col_pos
  done

let move_to_head col =
  let pb = col.problem in
  let head = pb.head in
  pb.head <- head + 1;
  (* Printf.printf "head from %d to %d\n" head pb.head; *)
  if col.pb_pos > head then (
    let ((col', _) as pair) = pb.cols.(head) in
    pb.cols.(head) <- pb.cols.(col.pb_pos);
    pb.cols.(col.pb_pos) <- pair;
    col'.pb_pos <- col.pb_pos;
    col.pb_pos <- head)

let remove_from_head col =
  let pb = col.problem in
  pb.head <- pb.head - 1

let move_to_tail col =
  let pb = col.problem in
  let tail = pb.tail - 1 in
  pb.tail <- tail;
  if col.pb_pos < tail then (
    let ((col', _) as pair) = pb.cols.(tail) in
    pb.cols.(tail) <- pb.cols.(col.pb_pos);
    pb.cols.(col.pb_pos) <- pair;
    col'.pb_pos <- col.pb_pos;
    col.pb_pos <- tail)

let remove_from_tail col =
  let pb = col.problem in
  pb.tail <- pb.tail + 1

let select row =
  disable_row row;
  for col_pos = 0 to row.rc_end - 1 do
    let col, _ = row.rc_arr.(col_pos) in
    (* disable_col col; *)
    move_to_tail col
  done;
  for row_pos = 0 to row.rr_end - 1 do
    let row', _ = row.rr_arr.(row_pos) in
    disable_row row'
  done

let deselect row =
  for row_pos = row.rr_end - 1 downto 0 do
    let row', _ = row.rr_arr.(row_pos) in
    enable_row row'
  done;
  for col_pos = row.rc_end - 1 downto 0 do
    let col, _ = row.rc_arr.(col_pos) in
    remove_from_tail col
    (* ; enable_col col *)
  done;
  enable_row row

let cover col =
  disable_col col;
  move_to_head col

let uncover col =
  remove_from_head col;
  enable_col col

let print_col col =
  Printf.printf "Col %d : " col.col_name;
  for i = 0 to col.cr_end - 1 do
    Printf.printf " %s(%d)" (fst col.cr_arr.(i)).row_name (snd col.cr_arr.(i))
  done;
  Printf.printf " |";
  for i = col.cr_end to Array.length col.cr_arr - 1 do
    Printf.printf " %s(%d)" (fst col.cr_arr.(i)).row_name (snd col.cr_arr.(i))
  done;
  Printf.printf "\n"

let print_row row =
  Printf.printf "Row %s : " row.row_name;
  for i = 0 to row.rc_end - 1 do
    Printf.printf " %d(%d)" (fst row.rc_arr.(i)).col_name (snd row.rc_arr.(i))
  done;
  Printf.printf " |";
  for i = row.rc_end to Array.length row.rc_arr - 1 do
    Printf.printf " %d(%d)" (fst row.rc_arr.(i)).col_name (snd row.rc_arr.(i))
  done;
  Printf.printf "\n        ";
  for i = 0 to row.rr_end - 1 do
    Printf.printf " %s(%d)" (fst row.rr_arr.(i)).row_name (snd row.rr_arr.(i))
  done;
  Printf.printf " |";
  for i = row.rr_end to Array.length row.rr_arr - 1 do
    Printf.printf " %s(%d)" (fst row.rr_arr.(i)).row_name (snd row.rr_arr.(i))
  done;
  Printf.printf "\n"

let _print_problem pb =
  let aux col = Printf.printf " %d(%d)" col.col_name col.pb_pos in
  Printf.printf "Col_list:";
  for i = 0 to pb.head - 1 do
    aux (fst pb.cols.(i))
  done;
  Printf.printf " |";
  for i = pb.head to pb.tail - 1 do
    aux (fst pb.cols.(i))
  done;
  Printf.printf " |";
  for i = pb.tail to Array.length pb.cols - 1 do
    aux (fst pb.cols.(i))
  done;
  Printf.printf "\n\n";
  for i = 0 to Array.length pb.cols - 1 do
    print_col (fst pb.cols.(i))
  done;
  Printf.printf "\n";
  for i = 0 to Array.length pb.rows - 1 do
    print_row pb.rows.(i)
  done;
  Printf.printf "\n\n"

let check_problem pb =
  assert (pb.head >= 0);
  assert (pb.head <= pb.tail);
  assert (pb.tail <= Array.length pb.cols);
  Array.iteri
    (fun i (col, _) ->
      assert (col.pb_pos = i);
      assert (col.cr_end >= 0);
      assert (col.cr_end <= Array.length col.cr_arr))
    pb.cols;
  Array.iter
    (fun row ->
      assert (row.rc_end >= 0);
      assert (row.rc_end <= Array.length row.rc_arr);
      Array.iteri
        (fun i (col, j) ->
          assert (fst col.cr_arr.(j) == row);
          assert (snd col.cr_arr.(j) == i))
        row.rc_arr;
      assert (row.rr_end >= 0);
      assert (row.rr_end <= Array.length row.rr_arr);
      Array.iteri
        (fun i (row', j) ->
          assert (fst row'.rr_arr.(j) == row);
          assert (snd row'.rr_arr.(j) == i))
        row.rr_arr)
    pb.rows

let make ec =
  let problem = { head = 0; tail = 0; cols = [||]; rows = [||] } in
  (* Create cols and rows *)
  let cols_tbl =
    Ec.get_cols ec
    |> Seq.map (fun col ->
           ( col,
             { col_name = col; cr_end = 0; cr_arr = [||]; problem; pb_pos = 0 }
           ))
    |> Hashtbl.of_seq
  and rows_tbl =
    Ec.get_rows ec
    |> Seq.map (fun row ->
           ( row,
             {
               row_name = row;
               rc_end = 0;
               rc_arr = [||];
               rr_end = 0;
               rr_arr = [||];
             } ))
    |> Hashtbl.of_seq
  in
  (* Make problem's arrays *)
  problem.cols <-
    Ec.get_primary_cols ec
    |> Seq.map (Hashtbl.find cols_tbl)
    |> Seq.map (fun col -> (col, ref 0))
    |> Array.of_seq;
  problem.tail <- Array.length problem.cols;
  problem.rows <- Hashtbl.to_seq_values rows_tbl |> Array.of_seq;
  (* Make arrays *)
  let a_col = problem.cols.(0) |> fst and a_row = problem.rows.(0) in
  Ec.get_cols ec
  |> Seq.iter (fun col_id ->
         let col = Hashtbl.find cols_tbl col_id in
         col.cr_arr <-
           Array.make (Ec.get_rows_of_col ec col_id |> List.length) (a_row, 0));
  Ec.get_rows ec
  |> Seq.iter (fun row_id ->
         let row = Hashtbl.find rows_tbl row_id in
         row.rc_arr <-
           Array.make
             (Ec.get_primary_cols_of_row ec row_id |> List.length)
             (a_col, 0);
         row.rr_arr <-
           Array.make
             ((Ec.get_rows_of_row ec row_id |> List.length) - 1)
             (a_row, 0));
  (* Number cols *)
  Array.iteri (fun i (col, _) -> col.pb_pos <- i) problem.cols;
  (* Populate arrays *)
  Array.iter
    (fun (col, _) ->
      let rows =
        Ec.get_rows_of_col ec col.col_name |> List.map (Hashtbl.find rows_tbl)
      in
      List.iter
        (fun row ->
          col.cr_arr.(col.cr_end) <- (row, row.rc_end);
          row.rc_arr.(row.rc_end) <- (col, col.cr_end);
          col.cr_end <- col.cr_end + 1;
          row.rc_end <- row.rc_end + 1)
        rows)
    problem.cols;
  Array.iter
    (fun row ->
      let rows =
        Ec.get_rows_of_row ec row.row_name |> List.map (Hashtbl.find rows_tbl)
      in
      List.iter
        (fun row' ->
          if row'.row_name < row.row_name then (
            row.rr_arr.(row.rr_end) <- (row', row'.rr_end);
            row'.rr_arr.(row'.rr_end) <- (row, row.rr_end);
            row.rr_end <- row.rr_end + 1;
            row'.rr_end <- row'.rr_end + 1))
        rows)
    problem.rows;
  check_problem problem;
  problem

let count_solutions pb =
  if pb.head = pb.tail then 0
  else
    let cnt = ref 0 in
    let rec aux () =
      if pb.head = pb.tail then incr cnt
      else
        (* pick a col to cover *)
        let col_ref = ref (fst pb.cols.(pb.head)) and pos = ref (pb.head + 1) in
        let score_ref = ref !col_ref.cr_end in
        while !pos < pb.tail do
          let curr_col = fst pb.cols.(!pos) in
          if curr_col.cr_end < !score_ref then (
            col_ref := curr_col;
            score_ref := curr_col.cr_end);
          incr pos
        done;
        let col = !col_ref in
        cover col;
        for i = 0 to col.cr_end - 1 do
          let row = fst col.cr_arr.(i) in
          select row;
          aux ();
          deselect row
        done;
        uncover col
    in
    aux ();
    !cnt

let rec move_in pb =
  if pb.head < pb.tail then (
    (* pick a col to cover *)
    let col_ref = ref (fst pb.cols.(pb.head))
    and pos = ref (pb.head + 1) in
    let score_ref = ref !col_ref.cr_end in
    while !pos < pb.tail do
      let curr_col = fst pb.cols.(!pos) in
      if curr_col.cr_end < !score_ref then (
        col_ref := curr_col;
        score_ref := curr_col.cr_end);
      incr pos
    done;
    let col = !col_ref in
    cover col;
    snd pb.cols.(pb.head - 1) := 0;
    move_on pb)

and move_on pb =
  let col, pos_ref = pb.cols.(pb.head - 1) in
  if !pos_ref >= col.cr_end then (
    uncover (fst pb.cols.(pb.head - 1));
    move_out pb)
  else
    let row = fst col.cr_arr.(!pos_ref) in
    select row;
    move_in pb

and move_out pb =
  if pb.head > 0 then (
    let col, pos_ref = pb.cols.(pb.head - 1) in
    let row = fst col.cr_arr.(!pos_ref) in
    deselect row;
    incr pos_ref;
    move_on pb)

let count_solutions2 pb =
  let cnt = ref 0 in
  move_in pb;
  while pb.head > 0 do
    incr cnt;
    move_out pb
  done;
  !cnt

let _get_sol pb =
  let s = ref [] in
  for i = 0 to pb.head - 1 do
    let col, pos = pb.cols.(i) in
    let row, _ = col.cr_arr.(!pos) in
    s := (col.col_name, !pos, col.cr_end, row.row_name) :: !s
  done;
  (pb.head, !s)
