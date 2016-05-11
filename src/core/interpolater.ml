open Core.Std
module Array = Array


type z_fun = x_col:int -> y_col:int -> (float * float) array -> float

let first_min_dist : z_fun = fun ~(x_col:int) ~(y_col:int) dists ->
 let z, w_star, v_star = Array.foldi
      ~f:(fun i (z, w_star, v_star) (w, v) ->
        let z' = z +. v in
        if w < w_star then
          (z', w, v)
        else
          (z', w_star, v_star))
          ~init:(0.0, Float.infinity, Float.nan) dists in
  v_star


let exp_avg : z_fun = fun  ~(x_col:int) ~(y_col:int) dists ->
    let d_sum, v_sum = Array.fold
     ~f:(fun (d_sum, v_sum) (d, v) ->
        let exp_inv_d = exp (-.d) in
        let (d_sum, v_sum) as tuple =
          d_sum +. exp_inv_d,
        exp_inv_d *. v +. v_sum in
        (* printf "d=%f,v=%f,d_sum=%f,v_sum=%f\n" d v d_sum v_sum; *) tuple) ~init:(0.0, 0.0) dists in
    v_sum /. d_sum

let default_top_n = 3
let exp_avg_top_n ?(n=default_top_n) : z_fun = fun ~(x_col:int) ~(y_col:int) dists ->
    Gen.of_array dists |> Gen.sort ~cmp:(fun (x,y) (x',y') -> Float.compare x x')
    |> Gen.chunks n |> Gen.next |> function
    | None -> 0.0
    | Some top_n -> exp_avg ~x_col ~y_col top_n

type dist_fun = int -> x_col:int -> y_col:int -> float -> float-> float

let exp_mse_if_xy_col_else_super_exp_mse : dist_fun =
  fun (i:int) ~x_col ~y_col v v' ->
   if i = x_col || i = y_col then
    (exp ((v-.v')**2.0)) -. 1.0
  else
    exp (exp (v -. v')**2.0) -. 1.0

let exp_mse_if_xy_col : dist_fun =
  fun i ~x_col ~y_col v v' ->
  if i = x_col || i = y_col then
    (exp ((v-.v')**2.0)) -. 1.0
  else
    1.0

let mse_dist_if_xy_col : dist_fun =
  fun i ~x_col ~y_col v v' ->
  if i = x_col || i = y_col then
    (v-.v')**2.0
  else
    1.0

let mse_dist_if_xy_col_else_super_exp_mse : dist_fun =
  fun i ~x_col ~y_col v v' ->
  if i = x_col || i = y_col then
    (v-.v')**2.0
  else
    exp_mse_if_xy_col_else_super_exp_mse i ~x_col ~y_col v v'

let default_dist = exp_mse_if_xy_col_else_super_exp_mse

let _output ~x_col ~y_col ?(z_f=exp_avg) ~z_col ?stddev_col ?(dist=default_dist)
  ~(plot_point:float array) ~(data:float array array) =
  let arr_dist_map point row = Array.mapi ~f:(fun i e -> dist i ~x_col ~y_col e row.(i)) point in
  let arr_dist point (row:float array) =
    let arr = arr_dist_map point row in
    let dist_sum = Array.fold ~f:(+.) ~init:0.0 arr in
    let z_val = row.(z_col) in
    dist_sum, z_val in
  let dists = Array.map (arr_dist plot_point) data in
  let v_star = z_f ~x_col ~y_col dists in
  Array.append plot_point [|v_star|]

let verbose = false (*TODO expose*)
type t = {
  mins: float array;
  maxes: float array;
  dims: int array;
  widths: float array;
  inc:float;
  x_min: float;
  y_min: float;
  x_max: float;
  y_max: float;
  z_min: float;
  z_max: float;
  x_col: int;
  y_col: int;
  z_col: int;
  x_width:float;
  y_width:float;
  z_width:float;
  z: float array array}

let min_dims = 3
let with_inc ?(x_col=0) ?(y_col=1) ?z_col ?stddev_col ?dist ?inc ?z_f ~(data:float array array) =
  let maxes = Column.max (Gen.of_array data) in
  let mins = Column.min (Gen.of_array data) in
  let abs_sum = Gen.map (Array.map ~f:Float.abs) (Gen.of_array data) |> Column.max |> Array.fold ~f:((+.)) ~init:0.0 in
  let inc = match inc with Some inc -> inc | None -> max 0.01 (abs_sum/.100.) in
  let widths = Array.map2_exn maxes mins
      ~f:(fun maxx minx -> Float.abs (maxx -. minx)) in
  let dims = Array.map widths
      ~f:(fun width -> max min_dims (Float.to_int (width /. inc))) in
  let dimx = dims.(x_col) in
  let dimy = dims.(y_col) in
  let x_min = mins.(x_col) in
  let y_min = mins.(y_col) in
  let x_max = maxes.(x_col) in
  let y_max = maxes.(y_col) in
  let x_width = widths.(x_col) in
  let y_width = widths.(y_col) in
  let z = Array.make_matrix ~dimx ~dimy 0.0 in
  let z_col = match z_col with Some z_col -> z_col | None -> Array.length maxes - 1 in
  for i = 0 to dimx - 1 do
    for j = 0 to dimy - 1 do
      let plot_point = [|x_min +. Float.of_int i *. inc; y_min +. Float.of_int j *. inc|] in
      let z_i_j = _output ~x_col ~y_col ?z_f ~z_col ?stddev_col ?dist ~plot_point ~data |> Array.last in
      if verbose then printf "[model-vis.interpolater] z[%d][%d]=%f\n" i j z_i_j else ();
      z.(i).(j) <- z_i_j
    done
  done;
  let z_max = maxes.(z_col) in
  let z_min = mins.(z_col) in
  let z_width = widths.(z_col) in
  {maxes;mins;dims;widths;inc;
  x_col;y_col;z_col;
  x_width;y_width;z_width;
  x_min;y_min;x_max;y_max;
  z_min;z_max;z}
