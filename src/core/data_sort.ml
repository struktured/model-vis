open Core.Std

module Make(C : sig type t [@@deriving ord] end) = struct

end

module Float = Core.Std.Float
module Array = Core.Std.Array
module Option = Core.Std.Option
module List = Core.Std.List

let sort_floats ?(cmp=Float.compare) (sampler:Sampler.t) =
    Gen.sort ~cmp:(Array.compare cmp) sampler

let with_min_max ?(cmp=Float.compare) (sampler:Sampler.t) =
  Gen.map (fun arr -> Array.min_elt ~cmp arr, Array.max_elt ~cmp arr, arr) sampler

let min_max ?cmp sampler = with_min_max ?cmp sampler |>
  Gen.map (fun (min,max,_) -> min,max)

module Feature_compare(F:Feature.S) = struct
  let with_exclusions ?(cmp=Float.compare) ~to_exclude arr1 arr2 =
    let min_len = min (Array.length arr1) (Array.length arr2) in
    let rec helper i =
      if i < min_len then
        if List.exists ~f:((=) F.all.(i)) to_exclude then
          helper (i+1)
        else
          cmp arr1.(i) arr2.(i) |> function 0 -> helper (i+1) | c -> c
      else
        0 in
    helper 0
end

