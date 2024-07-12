type ('col, 'row) t = {
  mutable is_dirty : bool;
  cols : ('col, bool) Hashtbl.t;
  cols_of_rows : ('row, 'col list) Hashtbl.t;
  rows_of_cols : ('col, 'row list) Hashtbl.t;
  rows_of_rows : ('row, 'row list) Hashtbl.t;
}

let create () =
  {
    is_dirty = false;
    cols = Hashtbl.create 10;
    cols_of_rows = Hashtbl.create 10;
    rows_of_cols = Hashtbl.create 10;
    rows_of_rows = Hashtbl.create 10;
  }

let add_primary pb opt =
  assert (Hashtbl.mem pb.cols opt = false);
  pb.is_dirty <- true;
  Hashtbl.add pb.cols opt true

let add_secondary pb opt =
  assert (Hashtbl.mem pb.cols opt = false);
  pb.is_dirty <- true;
  Hashtbl.add pb.cols opt false

let add_shape pb shape options =
  assert (Hashtbl.mem pb.cols_of_rows shape = false);
  pb.is_dirty <- true;
  List.iter (fun opt -> assert (Hashtbl.mem pb.cols opt)) options;
  Hashtbl.add pb.cols_of_rows shape options

let get_cols pb = Hashtbl.to_seq_keys pb.cols

let get_primary_cols pb =
  Hashtbl.to_seq pb.cols
  |> Seq.filter_map (fun (col, b) -> if b then Some col else None)

let get_rows pb = Hashtbl.to_seq_keys pb.cols_of_rows

let get_primary_cols_of_row pb row =
  Hashtbl.find pb.cols_of_rows row |> List.filter (Hashtbl.find pb.cols)

let get_cols_of_row pb row = Hashtbl.find pb.cols_of_rows row

let compute_rows_of_cols pb =
  Hashtbl.reset pb.rows_of_cols;
  Hashtbl.iter (fun col _ -> Hashtbl.add pb.rows_of_cols col []) pb.cols;
  Hashtbl.iter
    (fun row cols ->
      List.iter
        (fun col ->
          let rows = Hashtbl.find pb.rows_of_cols col in
          Hashtbl.replace pb.rows_of_cols col (row :: rows))
        cols)
    pb.cols_of_rows

let compute_rows_of_rows pb =
  Hashtbl.reset pb.rows_of_rows;
  Hashtbl.iter
    (fun row cols ->
      cols
      |> List.map (Hashtbl.find pb.rows_of_cols)
      |> List.flatten |> List.sort_uniq compare
      |> Hashtbl.add pb.rows_of_rows row)
    pb.cols_of_rows

let compute pb =
  compute_rows_of_cols pb;
  compute_rows_of_rows pb;
  pb.is_dirty <- false

let get_rows_of_col pb col =
  if pb.is_dirty then compute pb;
  Hashtbl.find pb.rows_of_cols col

let get_rows_of_row pb col =
  if pb.is_dirty then compute pb;
  Hashtbl.find pb.rows_of_rows col
