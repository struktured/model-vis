open Oml
open Core.Std

let n0_gen = Statistics.Sampling.normal ~mean:(-1.0) ~std:1.0 ()
let n1_gen = Statistics.Sampling.normal ~mean:1.0 ~std:1.0 ()
let sum_sqr () = let x = n0_gen() in let y = n1_gen() in [|x;y;x**2.0+.y**2.0|]

module XY =
  struct
    type t = [`X | `Y|`Z][@@deriving enum]
    let of_string = function "X" -> Some `X| "Y" -> Some `Y | "Z" -> Some `Z | s -> None
    let to_string = function `X -> "X" | `Y -> "Y" | `Z -> "Z"
    let domain _ = `Range (Float.neg_infinity, Float.infinity)
  end

module XY_Frame = Data_frame.Enum.Make(XY)

module Plot = Surface_plot.Make(XY_Frame)

let plot ?(device=`qtwidget) ?(trials=1000) () =
  let data_stream = Gen.init ~limit:trials
    (fun _ -> sum_sqr ()) in
  Plot.create
  ~tag:"two-sqr-gauss"
  ~title:"Sum of Two Squared Gaussians"
  ~device
  ~feature1:`X ~feature2:`Y ~output:`Z data_stream
