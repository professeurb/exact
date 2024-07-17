open Exact

type col = P of int | W of int

let langford_ex n =
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

let lf1 n = langford n |> DC.make |> DC.count_solutions
let lf2 n = langford n |> DC.make |> DC.count_solutions2
let lf3 n = langford n |> DC2.count_solutions_c
let lf4 n = langford n |> DC2.make |> DC2.count_solutions

let time f x =
  let t1 = Sys.time () in
  let r = f x in
  let t2 = Sys.time () in
  (r, t2 -. t1)

let _ =
  for n = 5 to 15 do
    (* let r1, t1 = time lf1 n *)
    (* and r2, t2 = time lf2 n *)
    (* and r3, t3 = time lf3 n *)
    (* and r4, t4 = time lf4 n in *)
    let r3, t3 = time lf3 n in
    (* assert (r1 = r2); *)
    (* assert (r1 = r3); *)
    (* assert (r1 = r4); *)
    (* Printf.printf "%2d: %8d : %8f / %8f / %8f / %8f\n%!" n r1 t1 t2 t3 t4 *)
    Printf.printf "%2d: %8d : %8f\n%!" n r3 t3
  done

let _ =
  for n = 5 to 15 do
    let r1, t1 = time lf1 n
    and r2, t2 = time lf2 n
    and r3, t3 = time lf3 n
    and r4, t4 = time lf4 n in
    assert (r1 = r2);
    assert (r1 = r3);
    assert (r1 = r4);
    Printf.printf "%2d: %8d : %8f / %8f / %8f / %8f\n%!" n r1 t1 t2 t3 t4
  done
