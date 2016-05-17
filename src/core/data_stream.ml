open Core.Std

module type S =
sig
  type t
  val init : ?start:int -> ?stop:int ->
    (int -> float array) -> t
  val to_gen : t -> float array Gen.t
  val to_array : t -> float array array
  val of_gen : float array Gen.t -> t
  val of_array : float array array -> t
  val of_list : float array list -> t
  val of_row : float array -> t
  val of_col : float array -> t
  val is_row : t -> bool * t
  val is_col : t -> bool * t
  val degenerate : t -> bool * t
  val to_col : t -> float array option * t
  val to_row : t -> float array option * t
  val to_row_or_col : t -> float array option * t
  val empty : t
  val clone : t -> t * t
  val mapij : f:(i:int -> j:int -> float -> float) -> t -> t
  val map : f:(float array -> float array) -> t -> t
  val map2 : f:(float array -> float array -> float array) -> t -> t -> t
  val mapi : f:(int -> float array -> float array) -> t -> t
  val map_each : f:(float -> float) -> t -> t
  val map_each2 : f:(float -> float -> float) -> t -> t -> t
  val map_each2ij : f:(i:int -> j:int -> float -> float -> float) -> t -> t -> t
  val transpose : t -> t * t
end

let default_trials = 10000

module Impl : S with type t = float array Gen.t =
struct
  type t = float array Gen.t
  let init ?(start=0) ?(stop=Int.max_value) f
    = Gen.int_range start (stop-1) |> Gen.map f
  let of_list = Gen.of_list
  let of_row = Gen.singleton
  let of_col t = Gen.of_array t |> Gen.map (fun x -> [|x|])
  let to_gen t = t
  let of_gen t = t
  let of_array t = Gen.of_array t
  let to_array t = Gen.to_array t
  let empty = Gen.empty
  let clone t = Gen.map (fun x -> x, x) t |> Gen.unzip
  let is_col t = clone t |> fun (t, t') ->
    match Gen.next t' with
    | Some arr -> Array.length arr = 1, t
    | None -> true, t
  let is_row t = clone t |> fun (t, t') ->
    match Gen.next t' with
      | Some arr ->
        begin match Gen.next t' with Some _ -> false | None -> true end, t
      | None -> true, t
  let degenerate t = clone t |> fun (t,t') ->
    match Gen.next t' with
      | Some arr -> Array.length arr = 0, t
      | None -> true, t
  let map ~f = Gen.map f
  let map_each ~f = map ~f:(fun arr -> Array.map ~f arr)
  let mapi ~f t = Gen.zip (Gen.unfold (fun i -> Some (i,(i + 1))) 0)
       t |> Gen.map (fun (i, arr) -> f i arr)
  let mapij ~f = mapi ~f:(fun i arr ->
    Array.mapi (fun j x -> f ~i ~j x) arr)
  let map2 ~f = Gen.map2 f
  let map_each2 ~f = map2 ~f:(fun arr1 arr2 -> Array.map2_exn ~f arr1 arr2)
   let transpose t =
    let mat = to_array t in
    let trans = Lacaml_D.Mat.transpose_copy (Lacaml_D.Mat.of_array mat) |>
    Lacaml_D.Mat.to_array |> of_array in
    of_array mat, trans

  let to_row t = match is_row t with
    | true, t ->
      clone t |> fun (t, t') ->
        begin
          Gen.next t |> function
            | Some _ as some_row -> some_row
            | None -> Some (Array.empty ())
        end, t'
    | false, t -> None, t

  let to_col t = match is_col t with
    | true, t ->
      clone t |> fun (t, t') ->
        begin
          Gen.map (fun arr -> arr.(0)) t |> Gen.to_array |>
          Option.some
        end, t'
    | false, t -> None, t

  let to_row_or_col t = to_row t |> function
     | Some _ as s,t -> s,t
     | None, t -> to_col t

  let map_each2ij ~f (d_i:t) (d_j:t) : t =
    (to_row_or_col d_i, to_row_or_col d_j) |> function
    | ((Some d_i, _), (Some d_j, _)) ->
    begin
      Array.mapi ~f:(fun (i:int) r_v ->
      Array.mapi ~f:(fun (j:int) c_v ->
        Printf.printf "[map_each2ij] i=%d j=%d r_v=%f c_v=%f\n" i j r_v c_v;
        f ~i ~j r_v c_v) d_j) d_i |> of_array
    end
    | _ -> failwith("input data streams are not rows or columns")
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

