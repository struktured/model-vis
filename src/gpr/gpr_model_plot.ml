module Make(F:Feature.S) =
struct
module Model = Gpr_model.Make(F)
 let plot ~(data:Sampler.t) = ()
end

