open Plplot

type t = [`png | `pdf | `qtwidget]

let of_string s = String.lowercase s |> function
  | "png" -> Some `png
  | "pdf" -> Some `pdf
  | "qtwidget" -> Some `qtwidget
  | _ -> None
let to_string = function `png -> "png" | `pdf -> "pdf" | `qtwidget -> "qtwidget"

let file_ext = function `png -> Some "png" | `pdf -> Some "pdf" | `qtwidget -> None

let set t = plsdev (to_string t)
