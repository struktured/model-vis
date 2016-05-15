open Core.Std

module type S =
sig
  type t (*= float array Gen.t*)
  val to_gen : t -> float array Gen.t
  val to_array : t -> float array array
  val of_gen : float array Gen.t -> t
  val of_array : float array array -> t
  val of_list : float array list -> t
  val singleton : float array -> t
  val empty : t
  val clone : t -> t * t
  val mapij : f:(i:int -> j:int -> float -> float) -> t -> t
  val map : f:(float array -> float array) -> t -> t
  val map2 : f:(float array -> float array -> float array) -> t -> t -> t
  val mapi : f:(int -> float array -> float array) -> t -> t
  val map_each : f:(float -> float) -> t -> t
  val map_each2 : f:(float -> float -> float) -> t -> t -> t
end

let default_trials = 10000

module Impl : S with type t = float array Gen.t = 
struct
  type t = float array Gen.t
  let of_list = Gen.of_list
  let singleton = Gen.singleton
  let to_gen t = t
  let of_gen t = t
  let of_array t = Gen.of_array t
  let to_array t = Gen.to_array t
  let empty = Gen.empty
  let clone t = Gen.map (fun x -> x, x) t |> Gen.unzip
  let map ~f = Gen.map f
  let map_each ~f = map ~f:(fun arr -> Array.map ~f arr)
  let mapi ~f t = Gen.zip (Gen.unfold (fun i -> Some (i,(i + 1))) 0)
       t |> Gen.map (fun (i, arr) -> f i arr)
  let mapij ~f = mapi ~f:(fun i arr ->
    Array.mapi (fun j x -> f ~i ~j x) arr)
  let map2 ~f = Gen.map2 f
  let map_each2 ~f = map2 ~f:(fun arr1 arr2 -> Array.map2_exn ~f arr1 arr2)
end

include Impl

module Uniform(D:Data_frame.S) =
  struct
    let create ?(fixed=[]) ?(trials=default_trials) () : t =
      let rand = Array.map (fun t ->
        match List.find_map fixed
      ~f:(fun (feature,value) -> if t = feature then Some value else None) with
    | Some value -> value
    | None -> match D.domain t with
      | `Points p -> CCRandom.choose_return p
      | `Range (l, r) -> CCRandom.float_range l r) D.all in
      let dim = Array.length D.all in
      Gen.int_range 0 trials |>
      Gen.map (fun _ ->
        Array.init dim (fun i -> let feature_rand = rand.(i) in
        CCRandom.run feature_rand))
  end

