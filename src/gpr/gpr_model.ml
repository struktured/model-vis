open Core.Std

module Args = struct
  type cmd = [ `Test ]

  type t = {
    cmd : cmd;
    model_file : string;
    with_stddev : bool;
    predictive : bool;
    max_iter : int option;
    n_inducing : int;
    sigma2 : float;
    amplitude : float;
    dim_red : int option;
    log_het_sked : float option;
    multiscale : bool;
    tol : float;
    step : float;
    eps : float;
    verbose : bool;
    samples: int option;
  }

  let cmd : cmd ref = ref `Test
  let model_file = ref None
  let with_stddev = ref false
  let predictive = ref false
  let max_iter = ref None
  let n_inducing = ref 10
  let sigma2 = ref 1.
  let amplitude = ref 1.
  let dim_red = ref None
  let log_het_sked = ref None
  let multiscale = ref false
  let tol = ref 0.1
  let step = ref 0.1
  let eps = ref 0.1
  let verbose = ref false
  let samples = ref None


  let set_some n_ref n = n_ref := Some n

  let args =
    Arg.align
      [
        (
          "-cmd",
          Arg.Symbol ([ "test"], function
            | "test" -> cmd := `Test
            | _ -> assert false  (* impossible *)
          ),
          " the default and only option is to test the model"
        );(
          "-model",
          Arg.String (fun str -> model_file := Some str),
          " model file to use"
        );(
          "-with-stddev",
          Arg.Set with_stddev,
          " make predictions with both mean and standard deviation"
        );(
          "-predictive",
          Arg.Set predictive,
          " standard deviation includes noise level (predictive distribution)"
        );(
          "-max-iter",
          Arg.Int (set_some max_iter),
          " maximum number of optimization steps (default: limitless)"
        );(
          "-n-inducing",
          Arg.Set_int n_inducing,
          sprintf
            " sets number of randomly initialized inducing inputs (default: %d)"
            !n_inducing
        );(
          "-sigma2",
          Arg.Set_float sigma2,
          sprintf " sets initial noise level (default: %f)" !sigma2
        );(
          "-amplitude",
          Arg.Set_float amplitude,
          sprintf " sets initial amplitude level (default: %f)" !amplitude
        );(
          "-dim-red",
          Arg.Int (set_some dim_red),
          " sets dimensionality reduction (default: none)"
        );(
          "-log-het-sked",
          Arg.Float (set_some log_het_sked),
          " turns on / sets log-heteroskedastic \
          noise (may require negative values)"
        );(
          "-multiscale",
          Arg.Set multiscale,
          " turns on multiscale approximation"
        );(
          "-tol",
          Arg.Set_float tol,
          sprintf " sets tolerance for gradient descent (default: %f)" !tol
        );(
          "-step",
          Arg.Set_float step,
          sprintf " sets step size for gradient descent (default: %f)" !step
        );(
          "-eps",
          Arg.Set_float eps,
          sprintf " sets epsilon for gradient descent (default: %f)" !eps
        );(
          "-verbose",
          Arg.Set verbose,
          " prints information while training"
        );(
          "-samples",
          Arg.Set verbose,
          "Number of samples when generating data points"
        );

      ]

  let usage_msg = sprintf "%s: -cmd [ test ] -model file" Sys.argv.(0)

  let anon_fun _ = failwith "no anonymous arguments allowed"

  let some name opt_ref =
    match !opt_ref with
    | Some v -> v
    | None ->
        eprintf "command line option %s not provided\n\n%!" name;
        prerr_endline usage_msg;
        exit 1

  let get () =
    Arg.parse args anon_fun usage_msg;
    {
      cmd = !cmd;
      model_file = some "model" model_file;
      with_stddev = !with_stddev;
      predictive = !predictive;
      max_iter = !max_iter;
      n_inducing = !n_inducing;
      sigma2 = !sigma2;
      amplitude = !amplitude;
      dim_red = !dim_red;
      log_het_sked = !log_het_sked;
      multiscale = !multiscale;
      tol = !tol;
      step = !step;
      eps = !eps;
      verbose = !verbose;
      samples = !samples;
    }
end

open Gpr

module GP = Fitc_gp.Make_deriv (Cov_se_fat.Deriv)
module FIC = GP.Variational_FIC.Eval

module Model = struct
  type t = {
    sigma2 : float;
    target_mean : float;
    input_means : vec;
    input_stddevs : vec;
    kernel : Cov_se_fat.Eval.Kernel.t;
    inducing_points : FIC.Spec.Inducing.t;
    coeffs : vec;
    co_variance_coeffs : FIC.Model.co_variance_coeffs;
  }
end

open Lacaml.D

exception Bailout

let read_test_samples sampler big_dim =
  let samples = Gen.to_array sampler in
  let n = Array.length samples in
  if n = 0 then Mat.empty
  else begin
    let input_dim = Array.length samples.(0) in
    if input_dim <> big_dim then
      failwithf
        "incompatible dimension of inputs (%d), expected %d"
        input_dim big_dim ();
    let inputs = Mat.create big_dim n in
    Array.iteri samples ~f:(fun c0 sample ->
      for r1 = 1 to big_dim do inputs.{r1, c0 + 1} <- sample.(r1 - 1) done);
    inputs
  end

let read_model model_file : Model.t =
  let ic = open_in model_file in
  let model = Marshal.from_channel ic in
  In_channel.close ic;
  model

module Uniform_sampler(F:Feature.S) =
struct
  module Default_sampler = Sampler.Uniform(F)
  let create args =
   Default_sampler.create ~trials:
     (Option.value ~default:Sampler.default_trials args.Args.samples) ()
end

module Make(Inputs:Feature.S)(Outputs:Feature.S) = struct
module In_out = Feature.In_out(Inputs)(Outputs)
let eval (sampler:Sampler.t) args : Sampler.t =
 let { Args.model_file; with_stddev; predictive} = args in
  let
    {
      Model.
      sigma2; target_mean; input_means; input_stddevs; kernel;
      inducing_points; coeffs; co_variance_coeffs
    } = read_model model_file
  in
  let big_dim = Vec.dim input_stddevs in
  let raw_inputs = read_test_samples sampler big_dim in
  let n_inputs = Mat.dim2 raw_inputs in
  for i = 1 to big_dim do
    let mean = input_means.{i} in
    let stddev = input_stddevs.{i} in
    for j = 1 to n_inputs do
      raw_inputs.{i, j} <- (raw_inputs.{i, j} -. mean) /. stddev;
    done;
  done;
  let mean_predictor = FIC.Mean_predictor.calc inducing_points ~coeffs in
  let inducing = FIC.Inducing.calc kernel inducing_points in
  let inputs = FIC.Inputs.calc raw_inputs inducing in
  let means = FIC.Means.get (FIC.Means.calc mean_predictor inputs) in
  let renorm_mean mean = mean +. target_mean in
  let data_array = Mat.to_array raw_inputs in
  let gen : Sampler.t = Gen.of_array data_array in
  if with_stddev then
    let co_variance_predictor =
      FIC.Co_variance_predictor.calc kernel inducing_points co_variance_coeffs
    in
    let vars = FIC.Variances.calc co_variance_predictor ~sigma2 inputs in
    let vars = FIC.Variances.get ~predictive vars in
    let cnt = ref 0 in
    Gen.map (fun (arr:float array) ->
      let i = !cnt in cnt := !cnt + 1;
      Array.concat [arr; [|renorm_mean means.{i};sqrt vars.{i}|]]) gen
  else
   Gen.map2 (fun (arr:float array) mean ->
        Array.concat [arr; [|renorm_mean mean|]]) gen
      (Gen.of_array (Vec.to_array means))
end

module Make_with_stats(Inputs:Feature.S) =
  Make(Inputs)(Stat_feature.Stat_feature)
