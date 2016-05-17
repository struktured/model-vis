open Plplot
open Core.Std
module Make(F:Data_frame.S) =
struct
module F_compare = Data_sort.Feature_compare(F)
module Plot_filename = Plot_filename.Make(F)
module F_XY = Interpolater.XY(F)
let create
    ~(feature:F.t)
    ~(output:F.t)
    ?fname
    ?dist
    ?interp
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

  let raw_data : float array array = data_stream
    |> Data_sort.sort_floats |> Data_stream.to_array in
  let open F_XY in
  let module Dim = Interpolater.Dim in
  let module Vector = Interpolater.Vector in
  let {x_dim;y_dim;dims} as interp =
    F_XY.apply ~x_col:feature ~y_col:output
    ?dist ?inc ?stddev_col:stddev ?interp raw_data in
  print_endline "interp'ed";
  let x_max = x_dim.Dim.max in
  let y_max = y_dim.Dim.max in
  let x_min = x_dim.Dim.min in
  let y_min = y_dim.Dim.min in
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;
  plwind x_min x_max y_min y_max;
  plpsty 0;
  let x = Vector.to_array_exn x_dim.Dim.points in
  let y = Vector.to_array_exn y_dim.Dim.points in
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
    ?interp
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
        create ?fname:None ?tag ~feature ?interp
          ?device ?title ~output ?stddev (Data_stream.of_array data))
          all_but_outputs
end


