open Core.Std

module Make(Columns:Feature.S) = struct
  module Columns = Columns
end

let of_file filename : Sampler.t =
    let file_chan = In_channel.create filename in
    Csv.of_channel file_chan |>
    Gen.unfold (fun chan -> 
        try 
          let arr = Csv.next chan |> List.map ~f:Float.of_string |>
                    Array.of_list in Some (arr, chan) 
        with _ -> In_channel.close file_chan;None)
let to_file sampler filename =
    let file_chan = Out_channel.create filename in
    (* TODO streaming version of this *)
    let samples = Gen.to_array (Gen.map (Array.map ~f:Float.to_string) sampler) in
    let csv =  Csv.of_array samples in
    Csv.output_all (Csv.to_channel file_chan) csv


