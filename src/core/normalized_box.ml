let default_padding = 0.99

module Box = struct type t=  {basex:float;basey:float;height:float} [@@deriving ord,show,make] 

  let _diag ~basex ~basey ~height =
    let sum = basex**2.0 +. basey**2.0 in
    sqrt sum

  let diag t = _diag ~basex:t.basex ~basey:t.basey ~height:t.height

  let world_coord ~diag ~other_coord =
  let d2, oc2 = (diag**2.0, other_coord**2.0) in
   (d2 -. oc2) |> sqrt
end

type box = Box.t [@@deriving ord,show]

let degree_ratio = 180. /. Oml.Util.pi
let to_degrees x = x *. degree_ratio
let to_radians x = x /. degree_ratio

let alt_ratio_of x = cos x
let alt_angle_of x = acos x

let az_ratio_of x = sin x
let az_angle_of x = asin x


module Coords = struct
  type t = {xmin:float;xmax:float;ymin:float;ymax:float} [@@deriving show,ord, make]
  let wind_of_world ?(padding=default_padding) ?alt ~zmin ~zmax t =
   let x_len = t.xmax -. t.xmin in
    let y_len = t.ymax -. t.ymin in
    let z_len = zmax -. zmin in
    let basey = padding*.y_len in
    let basex = padding*.x_len in
    let alt_rad, ratio = match alt with
    | Some alt ->
        let rad = to_radians alt in rad, alt_ratio_of rad
    | None ->
      begin
        let ratio = y_len /. z_len in
        if ratio < 1.0 then alt_angle_of ratio,ratio else alt_angle_of 0.5, 0.5
      end in
    let height = padding*.z_len in
    let box = Box.make ~basex ~basey ~height in
    let diag = Box.diag box in
    let xmin,xmax = -.diag/.2.0, diag/.2.0 in
    let ymin,ymax = -.diag/.2.0, diag/.2.0 in
    {xmin;xmax;ymin;ymax}, box, to_degrees alt_rad
end

type coords = Coords.t [@@deriving ord,show]

module Opt = CCOpt
module type S =
sig

  type t = {
    wind_2d:coords;
    world_2d:coords;
    box:box;
    zmin:float;
    zmax:float}
  [@@deriving show, ord, make]

  val plwind : t -> unit
  val plw3d : ?alt:float -> ?az:float -> t -> unit
  val plwall : ?alt:float -> ?az:float -> t -> unit
end

module Make (*: S*) =
struct
  let default_altitude = 45.
  let default_azimuth = 0. (* TODO auto tune *)
  let inc = 0.05

  type t = {
    wind_2d:coords;
    world_2d:coords;
    box:box;
    alt:float;
    zmin:float;
    zmax:float}
  [@@deriving show, ord, make]

(*
  let from_box ~basex ~basey ~height =
    {
      xmin = (-.basex) /. 2.0;
      xmax=basex /. 2.0;
      ymin=(-.basey) /. 2.0;
      ymax=basey /. 2.0;
      zmin = 0.;
      zmax = height;
      basex;
      basey;
      height
    }
*)
  let of_world ?(padding=default_padding) ?alt ~xmin ~xmax ~ymin ~ymax ~zmin ~zmax =
    let world_2d = Coords.{xmin;xmax;ymin;ymax} in
    let wind_2d,box,alt = Coords.wind_of_world ?alt ~padding world_2d ~zmin ~zmax in
    {
      wind_2d;world_2d;box;zmin;zmax;alt
    }


  let plwind t =
  let open Coords in
  Plplot.plwind t.wind_2d.xmin t.wind_2d.xmax t.wind_2d.ymin t.wind_2d.ymax

  let plw3d ?alt ?(az=default_azimuth) t =
    let open Box in
    let open Coords in
   Plplot.plw3d
    t.box.basex t.box.basey t.box.height
    t.world_2d.xmin t.world_2d.xmax
    t.world_2d.ymin t.world_2d.ymax
    t.zmin t.zmax
    (Core.Std.Option.value ~default:t.alt alt) az

  let plwall ?alt ?az t =
    plwind t;
    plw3d ?alt ?az t

end
include Make
