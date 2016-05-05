open Plplot
open Core.Std


let time_str () =
  let now = CalendarLib.Date.today () in
  CalendarLib.Printer.Date.to_string now

module Make(F: Feature.S) =
struct
type t = string

let or_default ?tag ~feature1 ?feature2 ~output ~device t_opt =
  match Plot_device.file_ext device with None -> None 
  | Some file_ext ->
  match t_opt with
  | Some t -> Some t
  | None ->
      let tag = match tag with Some t -> t ^ "." | None -> time_str() ^ "." in
      match feature2 with
      | Some feature2 -> Some (Printf.sprintf "%s%s-vs-%s-vs-%s.%s" tag
                  (F.to_string feature1) (F.to_string feature2) (F.to_string output) file_ext)
      | None -> Some (Printf.sprintf "%s%s-vs-%s.%s" tag (F.to_string feature1) (F.to_string output) file_ext)

let set = plsfnam

let set_or_default ?tag ~feature1 ?feature2 ~output ~device t_opt =
  or_default ?tag ~feature1 ?feature2 ~output ~device t_opt |>
    function Some s -> set s | None -> ()
end

