open Plplot
open Core.Std

let default_ns = 30           (* Default number of shade levels *)
let shedge ~ns ~zmin ~zmax =
    Array.init (ns + 1) (
      fun i ->
        zmin +. (zmax -. zmin) *. Float.of_int i /. Float.of_int ns
    )

module Make(F:Data_frame.S) =
struct
module F_compare = Data_sort.Feature_compare(F)
module Plot_filename = Plot_filename.Make(F)
module F_XYZ = Interpolater.XYZ(F)
let colorbar ~inc ?color ?contour values ~(output:F.t) =
  (* Smaller text *)
  plschr 0.0 0.5;
  (* Small ticks on the vertical axis *)
  plsmaj 0.0 inc;
  plsmin 0.0 inc;

  let axis =
    [
      `frame0;
      `frame1;
      `vertical_label;
      `unconventional_label;
      `major_ticks;
    ]
  in
  let shade = Plot.shade_colorbar ~custom:true ~axis values in
  let pos = Plot.viewport_pos ~inside:false 0.005 0.0 in
  Plot.plot [
    Plot.colorbar ?color ?contour ~orient:(`top (0.0225, 0.925)) ~label:[`bottom (F.to_string output)] ~pos shade;
  ];

  (* Reset text and tick sizes *)
  plschr 0.0 1.0;
  plsmaj 0.0 1.0;
  plsmin 0.0 1.0


let create
    ~(feature1:F.t)
    ~(feature2:F.t)
    ~(output:F.t)
    ?fname
    ?dist
    ?alt
    ?az
    (*?base_x
    ?base_y
    ?height*)
    ?interp
    ?inc
    ?(style=[])
    ?(device=`png)
    ?tag
    ?title
    ?(cont_width)
    ?(ns=default_ns)
    ?(stddev:F.t option)
    (data_stream:Data_stream.t) =

  Plot_filename.set_or_default ?tag ~feature1 ~feature2 ~output ~device fname;
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
  let open F_XYZ in
  let module Dim = Interpolater.Dim in
  let module Vector = Interpolater.Vector in
  let {x_dim;y_dim;z_dim;dims} as interp (*{z;x_min;y_min;x_max;y_max;z_min;z_max;inc} *) =
    F_XYZ.apply ~x_col:feature1 ~y_col:feature2 ~z_col:output
    ?dist ?inc ?stddev_col:stddev ?interp raw_data in
  let zmax = z_dim.Dim.max in
  let xmax = x_dim.Dim.max in
  let ymax = y_dim.Dim.max in
  let zmin = z_dim.Dim.min in
  let xmin = x_dim.Dim.min in
  let ymin = y_dim.Dim.min in
  let inc = z_dim.Dim.inc in
  let z = Vector.as_matrix z_dim.Dim.points in
  let shedge = shedge ~ns ~zmin ~zmax in
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;

  let module N = Normalized_box in
  let box = N.of_world ~padding:1.25 ~xmin ~xmax ~ymin ~ymax ~zmin ~zmax in
  N.plwall ?alt ?az box;

  plpsty 0;
  let x = x_dim.Dim.points |> Vector.to_array_exn in
  let y = y_dim.Dim.points |> Vector.to_array_exn in

  plsurf3d x y z style shedge;

  colorbar ~inc ~output shedge;

  plcol0 1;
  plbox "bcnst" 0.0 0 "bcnstv" 0.0 0;
  plcol0 2;
  let feature1_axis_text = F.to_string feature1 in
  let feature2_axis_text = F.to_string feature2 in
  let title = match title with
    | Some t -> t
    | None -> let output_text = F.to_string output in
        sprintf "%s vs. %s vs. %s"
          feature1_axis_text feature2_axis_text output_text in
  pllab feature1_axis_text feature2_axis_text title;
  (* Clean up *)
  plend ();
  ()

let for_each_feature
    ?dist
    ?interp
    ?inc
    ?device
    ?tag
    ?title
    ?ns
    ~(output:F.t)
    ?(stddev:F.t option)
    (data_stream:Data_stream.t) =
    let all_but_outputs = Array.filter ~f:(fun x -> not (x = output || Some x = stddev)) F.all in
    let data = Data_stream.to_array data_stream in
    Array.iter
      ~f:(fun feature1 -> Array.iter ~f:(fun feature2 ->
        if (feature1 = feature2) then () else
        create ?ns ?fname:None ?tag ~feature1 ~feature2 ?dist ?interp ?inc ?device
          ?title ~output ?stddev (Data_stream.of_array data))
          all_but_outputs)
      all_but_outputs;
    ()


end


