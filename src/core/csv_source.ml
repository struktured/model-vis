open Core.Std

module Make(Columns:Data_frame.S) = struct
  module Columns = Columns
end

let of_file filename : Data_stream.t =
    let file_chan = In_channel.create filename in
    Csv.of_channel file_chan |>
    Gen.unfold (fun chan ->
        try
          let arr = Csv.next chan |> List.map ~f:Float.of_string |>
                    Array.of_list in Some (arr, chan)
        with _ -> In_channel.close file_chan;None)

let to_file data_stream filename =
    let file_chan = Out_channel.create filename in
    try
    (* TODO streaming version of this *)
    let samples = Gen.to_array (Gen.map (Array.map ~f:Float.to_string) data_stream) in
    let csv =  Csv.of_array samples in
    Csv.output_all (Csv.to_channel file_chan) csv
    with _ -> ();
    Out_channel.close file_chan

