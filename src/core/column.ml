open Core.Std
type t = float array

let maybe_assign f (data: Data_stream.t) =
  match Gen.next data with
  | None -> Array.empty ()
  | Some arr -> Array.copy arr |> fun values ->
    Gen.fold (fun values arr -> Array.iteri
      ~f:(fun i v ->
        match f i values.(i) v with
        | Some v -> values.(i) <- v
        | None -> ()) arr;values)
    values data

let max = maybe_assign (fun _ x y -> if x < y then Some y else None)
let min = maybe_assign (fun _ x y -> if x > y then Some y else None)
let sum = maybe_assign (fun _ x y -> Some (x +. y))
let avg = maybe_assign (fun i x y -> Some ((1.0 /. (Float.of_int i +. 1.0))*.(y -.x) +. x))
