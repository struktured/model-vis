open Core.Std
module type S =
sig
  type t

  val all : t array

  val get : t -> 'a array -> 'a option

  val to_string : t -> string
  val of_string : string -> t option

  val domain : t -> [`Range of float * float | `Points of float list]
  val min : t -> float
  val max : t -> float

  val sub_array : t list -> 'a array -> 'a array

  val to_int : t -> int

  val of_int : int -> t option
end

module type Enum_S =
sig
  type t [@@deriving enum]
  val to_string : t -> string
  val of_string : string -> t option
  val domain : t -> [`Range of float * float | `Points of float list]
end

module Enum_feature(Enum:Enum_S) : S with type t = Enum.t =
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
  let min t = match domain t with
   | `Range (l, _) -> l
   | `Points [] -> Float.infinity
   | `Points p -> List.sort Float.compare p |> List.hd_exn
  let max t = match domain t with
   | `Range (_, r) -> r
   | `Points [] -> Float.neg_infinity
   | `Points p -> List.sort (fun a b -> Float.compare b a) p |> List.hd_exn

  let sub_array features arr =
    Array.filter_mapi arr ~f:(fun i v -> if List.exists ~f:(fun f -> all.(i) = f) features then Some v else None)

  let to_int = to_enum
  let of_int = of_enum
end

module Generic_features(C: sig val count : int end) : S = struct  
  module E : Enum_S = struct
    type t = int
    let min = 0 
    let max = C.count
    let to_enum t = t
    let of_enum i = if i < max then Some i else None
    let to_string i = Printf.sprintf "Feature %d" i
    let of_string s = String.split ~on:' ' s |> 
                      function | ["Feature"; i_str] -> Some (Int.of_string i_str)
                               | _ -> None
    let domain _ = `Range (Float.neg_infinity, Float.infinity)
  end
  module Enum = Enum_feature(E)
  include Enum
end

