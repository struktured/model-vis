#!/usr/bin/env utop
#require "model-vis.gpr-support"
open Core.Std
module Inputs =
struct
  type t = [`TScore | `RScore | `MetaT | `DefaultEqualsEffective]
    [@@deriving show,ord,enum]

  let to_string = show
  let of_string _ = None

  let domain = function | `TScore | `RScore | `MetaT -> `Range (0.0,1.0)
                      | `DefaultEqualsEffective -> `Points [0.0;1.]
end


module Inputs = Feature.Enum_feature(Inputs)
module Outputs =
struct
  type t = [
      | `UnaryScore (* mean *)
      | `ConfidenceScore (* std dev *)]
      [@@deriving enum,show,ord]

  let to_string = show
  let of_string _ = None
  let domain = function _ -> `Range (0.0,1.0)
end

module Outputs = struct
  include Feature.Enum_feature(Outputs)
end

module Uni = Sampler.Uniform(Inputs)

module G_plot = Gpr_model_plot.Make(Inputs)(Outputs)
module In_out = G_plot.Model.In_out
module S_plot = Surface_plot.Make(In_out)
let input_sampler ?trials () = Uni.create ?trials ()
let plot ?(fname="uniform-samples.csv") ?trials () =
  let data = input_sampler ?trials () in
  let inputs_outputs = Gen.to_array @@ G_plot.plot ?args:None ~data in
  let () = Csv_parser.to_file (Gen.of_array inputs_outputs) fname in
  let output = `Out `UnaryScore in
  let stddev = `Out `ConfidenceScore in
  S_plot.for_each_feature
    ~dist:Interpolater.mse_dist
    ?title:None
    ?device:None
    ~output
    ~z_f:(Interpolater.exp_avg_top_n ~n:20)
    ?ns:None
    ~stddev
    ~tag:"Unary Score Regression"
    ~inc:0.025
    ~sampler:(Gen.of_array inputs_outputs)

let () =
  plot ~trials:1000 ()
