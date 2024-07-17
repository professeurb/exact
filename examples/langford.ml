open Exact

type col = P of int | W of int

let langford n =
  let ec = EC.create () in
  for w = 1 to n do
    EC.add_primary ec (1000 + w)
  done;
  for x = 1 to 2 * n do
    EC.add_primary ec x
  done;
  for x = 1 to n - 1 do
    EC.add_shape ec (Printf.sprintf "%d,%d" x 1) [ 1000 + 1; x; x + 2 ]
  done;
  for w = 2 to n do
    for x = 1 to (2 * n) - w - 1 do
      EC.add_shape ec (Printf.sprintf "%d,%d" x w) [ 1000 + w; x; x + w + 1 ]
    done
  done;
  ec

let lf n = langford n |> DC.count_solutions

let time f x =
  let t1 = Sys.time () in
  let r = f x in
  let t2 = Sys.time () in
  (r, t2 -. t1)

let _ =
  for n = 5 to 15 do
    let r, t = time lf n in
    Printf.printf "%2d: %8d : %8f\n%!" n r t
  done
