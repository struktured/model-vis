module Make(Inputs:Feature.S)(Outputs:Feature.S) =
struct
  module Model = Gpr_model.Make(Inputs)(Outputs)
  let plot ?args ~(data:Sampler.t) = 
    let args = match args with Some a -> a | None -> Gpr_model.Args.get() in
    Model.eval data args 
end

module Make_with_stats(Inputs:Feature.S) = 
struct
  include Make(Inputs)(Stat_feature)
end



