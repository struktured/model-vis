open Plplot
open Core.Std

module Make(F:Data_frame.S) =
struct
module F_compare = Data_sort.Feature_compare(F)
module Plot_filename = Plot_filename.Make(F)
let create
    ~(feature:F.t)
    ~(output:F.t)
    ?fname
    ?(device=`png)
    ?tag
    ?title
    ?(stddev:F.t option)
    (data_stream:Data_stream.t) =

  Plot_filename.set_or_default ?tag ~feature1:feature ?feature2:None ~output ~device fname;
  Plot_device.set device;
  (* Parse and process command line arguments *)
  plparseopts Sys.argv [PL_PARSE_SKIP];

  (* Load color palettes *)
  plspal0 "cmap0_black_on_white.pal";
  plspal1 "cmap1_blue_yellow.pal" true;

  (* Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display *)
  plscmap0n 8;

    (* Initialize plplot *)
  plinit ();
  let raw_data : float array array = data_stream
    |> Data_sort.sort_floats |> Data_stream.to_array in
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;
  let (x_max,y_max) = Column.max @@ Data_stream.of_array raw_data |>
  fun arr -> F.get feature arr |> Option.value ~default:Float.neg_infinity,
    F.get output arr |> Option.value ~default:Float.neg_infinity in
  let (x_min,y_min) = Column.min @@ Data_stream.of_array raw_data |>
  fun arr -> F.get feature arr |> Option.value ~default:Float.infinity,
    F.get output arr |> Option.value ~default:Float.infinity in
  plwind x_min x_max y_min y_max;
  plpsty 0;

  let x = F.sub_array [feature] raw_data |> Gen.of_array |>
    Gen.map (function [|x|] -> x | _ -> failwith("not expected")) |> Gen.to_array in
  let y = F.sub_array [output] raw_data |> Gen.of_array |>
    Gen.map (function [|x|] -> x | _ -> failwith("not expected")) |> Gen.to_array in
  plline x y;

  plcol0 1;
  plbox "bcnst" 0.0 0 "bcnstv" 0.0 0;
  plcol0 2;
  let feature_axis_text = F.to_string feature in
  let output_text = F.to_string output in
  let title = match title with
    | Some t -> t
    | None -> sprintf "%s vs. %s"
          feature_axis_text output_text in
  pllab feature_axis_text output_text title;
  (* Clean up *)
  plend ();
  ()

  let for_each_feature
    ?dist
    ?inc
    ?device
    ?tag
    ?title
    ~(output:F.t)
    ?(stddev:F.t option)
    (data_stream:Data_stream.t) =
    let all_but_outputs = Array.filter ~f:(fun x -> not (x = output || Some x = stddev)) F.all in
    let data = Data_stream.to_array data_stream in
    Array.iter
      ~f:(fun feature ->
        create ?fname:None ?tag ~feature
          ?device ?title ~output ?stddev (Data_stream.of_array data))
          all_but_outputs
end


