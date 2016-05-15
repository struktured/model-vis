module Make(Inputs:Data_frame.S)(Outputs:Data_frame.S) =
struct
  module Model = Gpr_model.Make(Inputs)(Outputs)
  let plot ?args ~(data:Data_stream.t) =
    let args = match args with Some a -> a | None -> Gpr_model.Args.get() in
    Model.eval data args
end

module Make_with_stats(Inputs:Data_frame.S) =
struct
  include Make(Inputs)(Stat_frame)
end



