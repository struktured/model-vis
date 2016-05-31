open Oml
open Core.Std

let n0_gen = Statistics.Sampling.normal ~mean:(-1.0) ~std:1.0 ()
let n1_gen = Statistics.Sampling.normal ~mean:1.0 ~std:1.0 ()
let sum_sqr () = let x = n0_gen() in let y = n1_gen() in [|x;y;x**2.0+.y**2.0|]
let default_device = `qtwidget
let defaul_trials = 500
module XY =
  struct
    type t = [`X | `Y | `Z] [@@deriving enum]
    let of_string = function "X" -> Some `X| "Y" -> Some `Y | "Z" -> Some `Z | s -> None
    let to_string = function `X -> "X" | `Y -> "Y" | `Z -> "Z"
    let domain _ = `Range (Float.neg_infinity, Float.infinity)
  end

module XY_Frame = Data_frame.Enum.Make(XY)

module Surface_plot = Surface_plot.Make(XY_Frame)
let surface_plot ?(device=default_device) ?(trials=defaul_trials) () =
  let data_stream = Gen.init ~limit:trials
    (fun _ -> sum_sqr ()) in
  Surface_plot.for_each_feature
  ~tag:"two-sqr-gauss-surf"
  ~title:"Sum of Two Squared Gaussians (Surface Plot)"
  ~device
  ~output:`Z data_stream

module Surface_3d_plot = Surface_3d_plot.Make(XY_Frame)

let surface_3d_plot
  ?(device=default_device) ?(trials=defaul_trials) ?alt ?az () =
  let data_stream = Gen.init ~limit:trials
    (fun _ -> sum_sqr ()) in
  Surface_3d_plot.for_each_feature
  ?alt
  ?az
  ~tag:"two-sqr-gauss-surf-3d"
  ~title:"Sum of Two Squared Gaussians (3d Surface Plot)"
  ~device
  ~output:`Z data_stream

module Line_plot = Line_plot.Make(XY_Frame)

let line_plot
  ?(device=default_device) ?(trials=defaul_trials) () =
  let data_stream = Data_stream.init ~stop:trials
    (fun _ -> sum_sqr ()) in
  Line_plot.for_each_feature
  ~tag:"two-sqr-gauss-line"
  ~title:"Sum of Two Squared Gaussians (Line Plot)"
  ~device
  ~output:`Z data_stream
