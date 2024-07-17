(*  TODO: Add secondary colors *)

external forward_c :
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit
  = "forward"

external backward_c :
  (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit
  = "backward"

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
  if debug then Printf.printf "!ptr = %d / %d\n" !ptr (problem_size ec);
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
          pb.(col_pos + 1) <- row_addr;
          pb.(row_pos) <- col_pos;
          pb.(row_pos + 1) <- col_addr;
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
            pb.(row_pos + 1) <- row'_addr;
            pb.(row'_pos) <- row_pos;
            pb.(row'_pos + 1) <- row_addr;
            pb.(row_addr) <- row_pos + 2;
            pb.(row'_addr) <- row'_pos + 2))
        (Ec.get_rows_of_row ec row))
    (Ec.get_rows ec);
  pb

let make_c ec =
  let pb =
    Bigarray.Array1.init Int64 Bigarray.c_layout (problem_size ec) (fun _ ->
        Int64.zero)
  in
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
  if debug then Printf.printf "!ptr = %d / %d\n" !ptr (problem_size ec);
  (* assert (!ptr = Array.length pb); *)
  (* Fill primary colList *)
  pb.{0} <- 2 |> Int64.of_int (* head *);
  let tail = ref 2 in
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      pb.{!tail} <- col_addr |> Int64.of_int;
      pb.{col_addr - 1} <- !tail |> Int64.of_int;
      tail += 2)
    (Ec.get_primary_cols ec);
  pb.{1} <- !tail |> Int64.of_int;
  (* Fill cols_of_rows and rows_of_cols *)
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      pb.{col_addr} <- col_addr + 1 |> Int64.of_int)
    (Ec.get_primary_cols ec);
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      pb.{row_addr - 1} <- row_addr - 3 |> Int64.of_int)
    (Ec.get_rows ec);
  Seq.iter
    (fun col ->
      let col_addr = Hashtbl.find col_addresses col in
      List.iter
        (fun row ->
          let row_addr = Hashtbl.find row_addresses row in
          let col_pos = pb.{col_addr} |> Int64.to_int
          and row_pos = pb.{row_addr - 1} |> Int64.to_int in
          pb.{col_pos} <- row_pos |> Int64.of_int;
          pb.{col_pos + 1} <- row_addr |> Int64.of_int;
          pb.{row_pos} <- col_pos |> Int64.of_int;
          pb.{row_pos + 1} <- col_addr |> Int64.of_int;
          pb.{col_addr} <- col_pos + 2 |> Int64.of_int;
          pb.{row_addr - 1} <- row_pos - 2 |> Int64.of_int)
        (Ec.get_rows_of_col ec col))
    (Ec.get_primary_cols ec);
  (* Fill rows_of_rows *)
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      pb.{row_addr} <- row_addr + 1 |> Int64.of_int)
    (Ec.get_rows ec);
  Seq.iter
    (fun row ->
      let row_addr = Hashtbl.find row_addresses row in
      List.iter
        (fun row' ->
          if row' < row then (
            let row'_addr = Hashtbl.find row_addresses row' in
            let row_pos = pb.{row_addr} |> Int64.to_int
            and row'_pos = pb.{row'_addr} |> Int64.to_int in
            pb.{row_pos} <- row'_pos |> Int64.of_int;
            pb.{row_pos + 1} <- row'_addr |> Int64.of_int;
            pb.{row'_pos} <- row_pos |> Int64.of_int;
            pb.{row'_pos + 1} <- row_addr |> Int64.of_int;
            pb.{row_addr} <- row_pos + 2 |> Int64.of_int;
            pb.{row'_addr} <- row'_pos + 2 |> Int64.of_int))
        (Ec.get_rows_of_row ec row))
    (Ec.get_rows ec);
  (* let pb2 = make ec in *)
  (* Printf.printf "%d / %d\n%!" (Array.length pb2) (Bigarray.Array1.dim pb); *)
  (* for i = 0 to Array.length pb2 - 1 do *)
  (*   Printf.printf "%d %!" i; *)
  (*   assert (pb2.(i) = (pb.{i} |> Int64.to_int)) *)
  (* done; *)
  (* Printf.printf "Plum !\n%!"; *)
  let pb_addr = Obj.magic pb in
  for i = 0 to Bigarray.Array1.dim pb - 1 do
    pb.{i} <- Int64.add (Int64.mul (8 |> Int64.of_int) pb.{i}) pb_addr
  done;
  pb

let count_solutions pb =
  let arr = make_c pb and cnt = ref 0 in
  if arr.{0} = arr.{1} then 0
  else (
    (* Printf.printf "%d %d\n" (arr.{0} |> Int64.to_int) (arr.{1} |> Int64.to_int); *)
    forward_c arr;
    (* Printf.printf "%d %d\n" (arr.{0} |> Int64.to_int) (arr.{1} |> Int64.to_int); *)
    while arr.{0} = arr.{1} do
      incr cnt;
      backward_c arr
      (* Printf.printf "%d %d\n" (arr.{0} |> Int64.to_int) (arr.{1} |> Int64.to_int) *)
    done;
    !cnt)

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
