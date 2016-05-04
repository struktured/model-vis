open Core.Std
type t = float array Gen.t

let default_trials = 10000
module Uniform(Features:Feature.S) =
  struct
    let create ?(fixed=[]) ?(trials=default_trials) () : t =
      let rand = Array.map (fun t ->
        match List.find_map fixed
      ~f:(fun (feature,value) -> if t = feature then Some value else None) with
    | Some value -> value
    | None -> match Features.domain t with
      | `Points p -> CCRandom.choose_return p
      | `Range (r, l) -> CCRandom.float_range l r) Features.all in
      let dim = Array.length Features.all in
      Gen.int_range 0 trials |>
      Gen.map (fun _ ->
        Array.init dim (fun i -> let feature_rand = rand.(i) in
        CCRandom.run feature_rand))
  end
