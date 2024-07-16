open Exact

let triangle n =
  let dlx = EC.create () in
  for y = 0 to n - 1 do
    for x = 0 to y do
      EC.add_primary dlx (x, y)
    done
  done;
  for y = 0 to n - 2 do
    for x = 0 to y do
      EC.add_shape dlx (x, y, 1) [ (x, y); (x, y + 1); (x + 1, y + 1) ]
    done
  done;
  for y = 1 to n - 2 do
    for x = 0 to y - 1 do
      EC.add_shape dlx (x, y, 2) [ (x, y); (x + 1, y); (x + 1, y + 1) ]
    done
  done;
  DC.make dlx

let time f x =
  let t1 = Sys.time () in
  let r = f x in
  let t2 = Sys.time () in
  (r, t2 -. t1)

let _ =
  for i = 2 to 25 do
    if i mod 3 <> 1 || true then (
      let pb = triangle i in
      let r1, t1 = time DC.count_solutions pb
      and r2, t2 = time DC.count_solutions2 pb in
      assert (r1 = r2);
      Printf.printf "%2d: %8d - %4.6f, %4.6f \n%!" i r1 t1 t2)
  done
