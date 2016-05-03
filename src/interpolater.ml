open Core.Std
module Array = Array

let _avg_output ?z_col ?(dist=fun x y -> (exp (x -. y))**2.0) ~(points:Sampler.t) ~(data:float array array) =
  let arr_dist_map point row = Array.mapi ~f:(fun i e -> dist e row.(i)) point in
  let arr_dist point (row:float array) =
    arr_dist_map point row |> fun arr ->
      let z_col = match z_col with
       | Some z_col -> z_col
       | None -> Array.length row - 1 in
         Array.fold ~f:(+.) ~init:0.0 arr, row.(z_col) in
  let mapper p =
    let dists = Array.map (arr_dist p) data in
    let z, v_star = Array.foldi
      ~f:(fun i (z, v_star) (w, v) -> (z +. (exp (-.w))), (exp (-.w)) *. v +. v_star) ~init:(0.0,0.0) dists in
    Array.append p [|v_star /. z|] in
  Gen.map mapper points


type t = {
  mins: float array;
  maxes: float array;
  dims: int array;
  x_col: int;
  y_col: int;
  z_col: int;
  z: float array array}

let with_inc ?(x_col=0) ?(y_col=1) ?z_col ?dist ?inc ~(data:float array array) =
  let maxes = Column.max (Gen.of_array data) in
  let mins = Column.min (Gen.of_array data) in
  let abs_sum = Gen.map (Array.map ~f:Float.abs) (Gen.of_array data) |> Column.sum |> Array.fold ~f:((+.)) ~init:0.0 in
  let inc = match inc with Some inc -> inc | None -> max 0.00001 (log abs_sum) in
  let dims = Array.map2_exn maxes mins
      ~f:(fun maxx minx -> max 1 (Float.to_int (Float.abs (maxx -. minx) /. inc))) in
  let dimx = dims.(x_col) in
  let dimy = dims.(y_col) in
  let minx = mins.(x_col) in
  let miny = mins.(y_col) in
  let z = Array.make_matrix ~dimx ~dimy 0.0 in
  for i = 0 to dimx do
    for j = 0 to dimy do
      let point = [|minx +. (Float.of_int i) *.inc; miny +. (Float.of_int j) *.inc|] in
      z.(i).(j) <-
        (_avg_output ?z_col ?dist ~points:(Gen.singleton point) ~data |> Gen.get_exn |> Array.last)
    done
  done;
  let z_col = match z_col with Some z_col -> z_col | None -> Array.length maxes - 1 in
  {maxes;mins;dims;x_col;y_col;z_col;z}
