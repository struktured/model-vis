module Stat =
struct
  type t = [`Mean | `Variance] [@@deriving enum]
  let to_string = function
    | `Mean -> "Mean"
    | `Variance -> "Variance"
  let of_string : string -> t option = function
    | "Mean" -> Some `Mean
    | "Variance" -> Some `Variance
    | s -> None
  let domain = function
    | `Mean -> `Range (neg_infinity, infinity)
    | `Variance -> `Range (neg_infinity, infinity)
end

include Feature.Enum_feature(Stat)
