(*
        plshade demo, using color fill.

        Maurice LeBrun
        IFS, University of Texas at Austin
        20 Mar 1994
*)

open Plplot

(* Fundamental settings.  See notes[] for more info. *)

let ns = 20             (* Default number of shade levels *)

module Make(F:Feature.S) =
struct
module F_compare = Data_sort.Feature_compare(F)

let colorbar ?color ?contour values ~(output:F.t) =
  (* Smaller text *)
  plschr 0.0 0.75;
  (* Small ticks on the vertical axis *)
  plsmaj 0.0 0.5;
  plsmin 0.0 0.5;

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
    Plot.colorbar ?color ?contour ~orient:(`top (0.0375, 0.875)) ~label:[`bottom (F.to_string output)] ~pos shade;
  ];

  (* Reset text and tick sizes *)
  plschr 0.0 1.0;
  plsmaj 0.0 1.0;
  plsmin 0.0 1.0


let create
    ?dist
    ?inc
    ?(device)
    ?title
    ~(feature1:F.t)
    ~(feature2:F.t)
    ~(output:F.t)
    ?(stddev:F.t option)
    ~(sampler:Sampler.t) =
  let fill_width = 2.0 in
  let cont_color = 0 in
  let cont_width = 0.0 in

  match device with Some d -> plsdev d | None -> ();

  (* Parse and process command line arguments *)
  plparseopts Sys.argv [PL_PARSE_FULL];

  (* Load color palettes *)
  plspal0 "cmap0_black_on_white.pal";
  plspal1 "cmap1_blue_yellow.pal" true;

  (* Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display *)
  plscmap0n 3;

    (* Initialize plplot *)
  plinit ();
  let raw_data : float array array = sampler
    |> Data_sort.sort_floats |> Gen.to_array in
  let open Interpolater in
  let {z;x_min;y_min;x_max;y_max;z_min;z_max} = with_inc ~x_col:(F.to_int feature1) ~y_col:(F.to_int feature2) ~z_col:(F.to_int output)
    ?dist ?inc ~data:raw_data in
  let shedge =
    Array.init (ns + 1) (
      fun i ->
        z_min +. (z_max -. z_min) *. float_of_int i /. float_of_int ns
    )
  in


  (* Plot using identity transform *)
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;
  (* TODO Round to nereast 1*10^(-x) based on relative precisions?? *)
  plwind x_min x_max y_min y_max;
  plpsty 0;

  plshades z x_min x_max y_min y_max shedge fill_width cont_color cont_width true;

  colorbar ~output shedge;

  plcol0 1;
  plbox "bcnst" 0.0 0 "bcnstv" 0.0 0;
  plcol0 2;
  let feature1_axis_text = F.to_string feature1 in
  let feature2_axis_text = F.to_string feature2 in
  let title = match title with
    | Some t -> t
    | None -> let output_text = F.to_string output in
        Printf.sprintf "%s (%s vs. %s)"
          feature1_axis_text feature2_axis_text output_text in
  pllab feature1_axis_text feature2_axis_text title;
  (* Clean up *)
  plend ();
  ()
end


