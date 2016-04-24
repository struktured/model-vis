type t = float array Gen.t

let default_trials = 10000
module Uniform(Features:Feature.S) =
struct
  let create ?(trials=default_trials) () : t =
  let rand = Array.map (fun t ->
    match Features.domain t with
    | `Points p -> CCRandom.choose_return p
    | `Range (r, l) -> CCRandom.float_range l r) Features.all in
  let dim = Array.length Features.all in
  Gen.int_range 0 trials |>
      Gen.map (fun _ ->
        Array.init dim (fun i -> let feature_rand = rand.(i) in
        CCRandom.run feature_rand))
end
