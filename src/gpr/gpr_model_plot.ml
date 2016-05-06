module Make(F:Feature.S) =
struct
  module Model = Gpr_model.Make_with_stats(F)
  let plot ?args ~(data:Sampler.t) = 
    let args = match args with Some a -> a | None -> Gpr_model.Args.get() in
    Model.eval data args 
end

