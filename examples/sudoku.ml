open Exact

type elt =
  | S
  | B of int * int
  | L of int * int
  | C of int * int
  | X of int * int

let grid str =
  let pb = EC.create () in
  EC.add_primary pb S;
  for i = 0 to 8 do
    for v = 1 to 9 do
      EC.add_secondary pb (B (i, v));
      EC.add_secondary pb (L (i, v));
      EC.add_secondary pb (C (i, v));
      EC.add_primary pb (X (i, v - 1))
    done
  done;
  for x = 0 to 8 do
    for y = 0 to 8 do
      let b = (x / 3) + (3 * (y / 3)) in
      for v = 1 to 9 do
        EC.add_shape pb (x, y, b, v) [ B (b, v); L (y, v); C (x, v); X (x, y) ]
      done
    done
  done;
  let start = ref [ S ] in
  for y = 0 to 8 do
    for x = 0 to 8 do
      let c = str.[(10 * y) + x] in
      if c <> '.' then (
        let b = (x / 3) + (3 * (y / 3)) and v = Char.code c - 48 in
        assert (v >= 1);
        assert (v <= 9);
        start := B (b, v) :: !start;
        start := L (y, v) :: !start;
        start := C (x, v) :: !start;
        start := X (x, y) :: !start)
    done
  done;
  EC.add_shape pb (0, 0, 0, 0) !start;
  pb

let _ =
  let pb =
    grid
      {|.837.16..
.4......5
....8....
....9.2..
.3.2.8.4.
..8.6....
...9.....
..13.27..
7......6.|}
  in
  Printf.printf "Number of solutions: %d - %d\n"
    (DC.make pb |> DC.count_solutions)
    (DC.make pb |> DC.count_solutions2)
