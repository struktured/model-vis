open Core.Std
module Array = Array


type interp = (float * float) array -> float

let first_min_dist : interp = fun dists ->
 let z, w_star, v_star = Array.foldi
      ~f:(fun i (z, w_star, v_star) (w, v) ->
        let z' = z +. v in
        if w < w_star then
          (z', w, v)
        else
          (z', w_star, v_star))
          ~init:(0.0, Float.infinity, Float.nan) dists in
  v_star

let exp_avg : interp = fun dists ->
    let d_sum, v_sum = Array.fold
     ~f:(fun (d_sum, v_sum) (d, v) ->
        let exp_inv_d = exp (-.d) in
        let (d_sum, v_sum) as tuple =
          d_sum +. exp_inv_d,
        exp_inv_d *. v +. v_sum in
        (* printf "d=%f,v=%f,d_sum=%f,v_sum=%f\n" d v d_sum v_sum; *) tuple) ~init:(0.0, 0.0) dists in
    v_sum /. d_sum

let default_top_n = 3
let exp_avg_top_n ?(n=default_top_n) : interp = fun  dists ->
    Gen.of_array dists |> Gen.sort ~cmp:(fun (x,y) (x',y') -> Float.compare x x')
    |> Gen.chunks n |> Gen.next |> function
    | None -> 0.0
    | Some top_n -> exp_avg top_n

type dist_fun = int -> float -> float -> float

let exp_mse : dist_fun =
  fun (i:int) v v' ->
    (exp ((v-.v')**2.0)) -. 1.0

let mse_dist : dist_fun =
  fun i v v' ->
    (v-.v')**2.0

let default_dist = exp_mse

let _output ?(interp=exp_avg) ~(x_col:int) ~(y_col:int) ?(z_col:int option) ?stddev_col ?(dist=default_dist)
  ~(point:float array) (data:float array array) =
  let arr_dist_map point row = Array.mapi ~f:(fun i e ->
      match i with
      | 0 -> dist i e row.(x_col)
      | 1 -> dist i e row.(y_col)
      | x -> failwith @@ sprintf "unexpected point size %d" x) point in
  let arr_dist point (row:float array) =
    let arr = arr_dist_map point row in
    let dist_sum = Array.fold ~f:(+.) ~init:0.0 arr in
    let output_col =
      begin match z_col with Some i -> i | None -> y_col end in
    dist_sum, row.(output_col) in
  let dists = Array.map (arr_dist point) data in
  let v_star = interp dists in
  Array.append point [|v_star|]

let verbose = true (*TODO expose*)

type axis = [`X | `Y | `Z] [@@deriving show, enum, ord]

module Vector =
struct
  type t = [`Row of float array | `Col of float array | `Matrix of float array array] [@@deriving ord,show]
let of_row x = `Row x
let of_col x = `Col x
let of_matrix x = `Matrix x

let to_array_exn : t -> float array = function
  | `Row x -> x
  | `Col x -> x
  | `Matrix _ -> failwith("not an array")

let to_matrix_exn : t -> float array array = function
  | `Matrix x -> x
  | _ -> failwith("not a matrix")

let to_data_stream : t -> Data_stream.t = function
  | `Matrix x -> Data_stream.of_array x
  | `Row x -> Data_stream.of_row x
  | `Col x -> Data_stream.of_col x

let as_matrix t = to_data_stream t |> Data_stream.to_array

end

type vector = Vector.t [@@deriving show,ord]

module Dim =
struct
type t =
{
  axis: axis;
  col: int;
  min: float;
  max: float;
  len: int;
  width: float;
  inc:float;
  points : vector;
  data : float array
} [@@deriving show, ord]
end

type dimension = Dim.t [@@deriving show,ord]

module Dims =
struct
type t =
{
  mins: float array;
  maxes: float array;
  lens: int array;
  widths: float array;
  incs : float array;
  data : float array array;
} [@@deriving show, ord]

let min_dims = 3
let of_data ?inc data =
  let maxes = Column.max (Gen.of_array data) in
  let data_len = Array.length maxes in
  let mins = Column.min (Gen.of_array data) in
  let abs_max = Gen.map (Array.map ~f:Float.abs) (Gen.of_array data) |> Column.max |> Array.fold ~f:((+.)) ~init:0.0 in
  let inc = match inc with Some inc -> inc | None -> (abs_max/.200.) in
  if inc = 0.0 then failwith("data has no mins or maxes") else
  let widths = Array.map2_exn maxes mins
      ~f:(fun maxx minx -> Float.abs (maxx -. minx)) in
  let lens = Array.map widths
      ~f:(fun width -> max min_dims (Float.to_int (width /. inc))) in
  let data_stream, data_stream_transpose = Data_stream.transpose (Gen.of_array data) in
  {maxes;mins;lens;widths;data=data_stream_transpose |> Data_stream.to_array;
  incs=Array.create ~len:data_len inc}

let to_dim1 ?points dims axis col =
  let len = dims.lens.(col) in
  let min = dims.mins.(col) in
  let inc = dims.incs.(col) in
  let points = match points with
    | Some p -> `Col p
    | None -> Gen.int_range 0 (len-1) |> Gen.fold_map
    (fun acc (_:int) -> acc +. inc) min
      |> Gen.to_array |> fun x -> `Col x in
  Dim.{axis;col;points;
    inc;
    len;
    min;
    max=dims.maxes.(col);
    width=dims.widths.(col);
    data=dims.data.(col)}

let to_dim2 dims axis col x_points y_points f =
  let len = dims.lens.(col) in
  let min = dims.mins.(col) in
  let inc = dims.incs.(col) in
  let points : vector = `Matrix (Data_stream.map_each2ij
  ~f:(fun ~i ~j x y -> f ~i ~j [|x;y|])
  (Vector.to_data_stream x_points)
  (Vector.to_data_stream y_points)
  |> Data_stream.to_array) in
  Dim.{axis;col;points;
    inc;
    len;
    min;
    max=dims.maxes.(col);
    width=dims.widths.(col);
    data=dims.data.(col)}
end

type dimensions = Dims.t [@@deriving show,ord]

module XYZ(F:Data_frame.S) =
struct
type t = {
  dims : dimensions;
  x_dim : dimension;
  y_dim : dimension;
  z_dim : dimension;
} [@@deriving show, ord]
module F = F
let apply ~x_col ~y_col ~z_col ?stddev_col ?dist ?inc ?interp (data:float array array) =
  let dims = Dims.of_data ?inc data in
  let open Dims in
  let x_dim = Dims.to_dim1 dims `X (F.to_int x_col) in
  let y_dim = Dims.to_dim1 dims `Y (F.to_int y_col) in
  let f ~i ~j point = _output ?interp ~x_col:(F.to_int x_col) ~y_col:(F.to_int y_col)
   ~z_col:(F.to_int z_col) ?stddev_col ?dist ~point data
    |> Array.last |> fun z ->
        if verbose then printf "[model-vis.interpolater] z[%d][%d]=%f\n" i j z else ();z in
  let z_dim = Dims.to_dim2 dims `Z (F.to_int z_col) x_dim.Dim.points y_dim.Dim.points f in
  {x_dim;y_dim;z_dim; dims}
end


module XY(F:Data_frame.S) =
struct
type t = {
  dims : dimensions;
  x_dim : dimension;
  y_dim : dimension;
} [@@deriving show, ord]
module F = F
let apply ?x_col ?y_col ?stddev_col ?dist ?inc ?interp (data:float array array) =
  let dims = Dims.of_data ?inc data in
  let open Dims in
  let x_col = match x_col with Some x_col -> F.to_int x_col | None -> 0 in
  let y_col = match y_col with Some y_col -> F.to_int y_col | None -> 1 in
  let x_dim = Dims.to_dim1 dims `X x_col in
  let f i point = _output ?interp ~x_col ~y_col ?z_col:None ?stddev_col ?dist ~point data
    |> Array.last |> fun y ->
        if verbose then printf "[model-vis.interpolater] y[%d]=%f\n" i y else ();y in
  let x_points = x_dim.Dim.points |> Vector.to_array_exn in
  let y_points = Array.mapi (fun i x -> f i [|x|]) x_points in
  let y_dim = Dims.to_dim1 ~points:y_points dims `Y y_col in
  {x_dim;y_dim;dims}
end
