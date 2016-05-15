open Core.Std

module Make(C : sig type t [@@deriving ord] end) = struct

end

module Float = Core.Std.Float
module Array = Core.Std.Array
module Option = Core.Std.Option
module List = Core.Std.List

let sort_floats ?(cmp=Float.compare) data_stream =
  Data_stream.to_gen data_stream |>
  Gen.sort ~cmp:(Array.compare cmp) |>
  Data_stream.of_gen

let with_min_max ?(cmp=Float.compare) (data_stream:Data_stream.t) =
  Data_stream.to_gen data_stream |>
  Gen.map (fun arr ->
    Array.append arr
    ([|Array.min_elt ~cmp arr; Array.max_elt ~cmp arr|]
    |> Array.filter_map ~f:(fun x -> x))) |>
  Data_stream.of_gen

let min_max ?cmp data_stream = with_min_max ?cmp data_stream |>
  Data_stream.to_gen |>
  Gen.map (fun arr -> Array.length arr |>
    function
      | 0 -> Array.empty ()
      | 1 -> [|arr.(0);arr.(0)|]
      | len -> [|arr.(len-1);arr.(len - 2)|]) |> 
  Data_stream.of_gen

module Feature_compare(F:Data_frame.S) = struct
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

