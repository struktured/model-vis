open Plplot
open Core.Std
let nyi x = failwith("nyi")
module Make(F:Data_frame.S) =
struct
module F_compare = Data_sort.Feature_compare(F)
module Plot_filename = Plot_filename.Make(F)
let create
    ~(feature:F.t)
    ~(output:F.t)
    ?fname
    ?dist
    ?z_f
    ?inc
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

  let _ : float array array = data_stream
    |> Data_sort.sort_floats |> Data_stream.to_array in
  (*let {z;x;y;x_min;y_min;x_max;y_max;z_min;z_max;inc} = 
    with_inc ~x_col:(F.to_int feature) ?y_col:None ~z_col:(F.to_int output)
    ?dist ?inc ?stddev_col:(Option.map ~f:F.to_int stddev) ?z_f ~data:raw_data *) 
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;
  (*plwind x_min x_max y_min y_max *); 
  plpsty 0;

  (*
  let x = F.sub_array [feature] raw_data |> Gen.of_array |>
    Gen.map (function [|x|] -> x | _ -> failwith("not expected")) |> Gen.to_array in
  let y = F.sub_array [output] raw_data |> Gen.of_array |>
    Gen.map (function [|x|] -> x | _ -> failwith("not expected")) |> Gen.to_array in
  plline x y;
 *)
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


