open Core.Std
module Array = Array
let _avg_output ?(dist=fun x y -> (exp (x -. y))**2.0) ~(points:Sampler.t) ~(data:float array array) : Sampler.t =
  let arr_dist_map point row = Array.mapi ~f:(fun i e -> dist e row.(i)) point in
  let arr_dist point (row:float array) =
    arr_dist_map point row |> fun arr -> Array.fold ~f:(+.) ~init:0.0 arr, row.(Array.length row - 1) in
  let mapper p =
    let dists = Array.map (arr_dist p) data in
    let z, v_star = Array.foldi
      ~f:(fun i (z, v_star) (w, v) -> (z +. (exp (-.w))), (exp (-.w)) *. v +. v_star) ~init:(0.0,0.0) dists in
    Array.append p [|v_star /. z|] in
  Gen.map mapper points


type box = {minx:float;maxx:float;miny:float;maxy:float}

let with_inc ?dist ?inc ~(data:float array array) =
  let {minx;miny;maxx;maxy} as box = failwith("nyi") in
  let inc = match inc with Some inc -> inc | None -> log (maxx +. maxy) in
  let dimx = max 1 (Float.to_int (Float.abs (maxx -. minx) /. inc)) in
  let dimy = max 1 (Float.to_int (Float.abs (maxy -. miny) /. inc)) in
  (*let slope = Float.of_int dimx /. Float.of_int dimy in*)
  let z = Array.make_matrix ~dimx ~dimy 0.0 in
  for i = 0 to dimx do
    for j = 0 to dimy do
      let xy = [|minx +. (Float.of_int i) *.inc; miny +. (Float.of_int j) *.inc|] in
      z.(i).(j) <-
        (_avg_output ?dist ~points:(Gen.singleton xy) ~data |> Gen.get_exn |> Array.last)
    done
  done;
  z,box






