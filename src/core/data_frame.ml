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

module Enum =
struct
module type DATA_FRAME = S
module type S =
sig
  type t [@@deriving enum]
  val to_string : t -> string
  val of_string : string -> t option
  val domain : t -> [`Range of float * float | `Points of float list]
end

module Make(Enum:S) : DATA_FRAME with type t = Enum.t =
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
end

module Generic =
struct
module type DATA_FRAME = S

module Attributes = struct
module type SIZE = sig val size : int end

module type PREFIX = sig val prefix : string end
module Default_prefix : PREFIX = struct let prefix = "Feature" end

module type DOMAIN = sig val domain : float * float end
module Default_domain : DOMAIN = struct let domain = 0.0, 1.0 end
module type S =
sig
  include SIZE
  include PREFIX
  include DOMAIN
end

module Make(Size:SIZE)(Prefix:PREFIX)(Domain:DOMAIN) : S =
struct
  let size = Size.size
  let prefix = Prefix.prefix
  let domain = Domain.domain
end

module Of_size(Size:SIZE) : S =
struct
  include Make(Size)(Default_prefix)(Default_domain)
end
end

module Make(Attr: Attributes.S) : DATA_FRAME with type t = int =
struct
  module E : Enum.S with type t = int = struct
    type t = int
    let min = 0
    let max = Attr.size
    let to_enum t = t
    let of_enum i = if i < max then Some i else None
    let to_string i = Printf.sprintf "Feature %d" i
    let of_string s = String.split ~on:' ' s |>
                      function | [label; i_str] when label=Attr.prefix -> Some (Int.of_string i_str)
                               | _ -> None
    let domain t = let l,r = Attr.domain in `Range (l, r)
  end
  module Enum = Enum.Make(E)
  include Enum
end
end

module In_out(In:S)(Out:S) : S with type t = [`In of In.t | `Out of Out.t] =
struct
  type t = [`In of In.t | `Out of Out.t]
  let in_len = Array.length In.all
  let out_len = Array.length Out.all
  let to_int = function `In t -> In.to_int t |`Out t -> Out.to_int t + in_len
  let of_int = function 
    | i when i < in_len && i >= 0 -> begin match In.of_int i with Some i -> 
      Some (`In i) | None -> None end
    | i when i >= in_len && i < in_len+out_len -> 
      begin 
        match Out.of_int (i-in_len)
        with Some o -> Some (`Out o) | None -> None 
      end
    | _ -> None
  let all = Array.concat [
      (Array.map In.all ~f:(fun x -> `In x));
      (Array.map Out.all ~f:(fun x -> `Out x))]

  let get t arr = to_int t |> function 
    | i when i >= 0 && i < Array.length arr -> Some arr.(i)
    | _ -> None  

  let to_string = function 
    |`In i -> In.to_string i
    |`Out o -> Out.to_string o

  let of_string x = 
    match In.of_string x with
    | Some i -> Some (`In i)
    | None -> match Out.of_string x with
      | Some o -> Some (`Out o)
      | None -> None

  let domain = function 
    | `In i -> In.domain i
    | `Out o -> Out.domain o

  let min = function
    | `In i -> In.min i
    | `Out o -> Out.min o

  let max = function
    | `In i -> In.max i
    | `Out o -> Out.max o


  let sub_array features arr =
    Array.empty ()

end
