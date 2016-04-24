module type S =
sig
  type t

  val all : t array

  val get : t -> 'a array -> 'a option

  val to_string : t -> string
  val of_string : string -> t option

  val domain : t -> [`Range of float * float | `Points of float list]
end

module Enum_feature(Enum:
sig
  type t [@@deriving enum]
  val to_string : t-> string
  val of_string : string -> t option
  val domain : t -> [`Range of float * float | `Points of float list]
end) : S with type t = Enum.t =
struct
  include Enum
  let all = Gen.int_range min max
    |> Gen.map of_enum
    |> Gen.filter_map CCFun.id
    |> Gen.to_array
  let get t arr = to_enum t |>
    function i when i < Array.length arr ->
      Array.get arr i |> CCOpt.return
  | _ -> None
end

