(*
        plshade demo, using color fill.

        Maurice LeBrun
        IFS, University of Texas at Austin
        20 Mar 1994
*)

open Plplot

let colorbar ?color ?contour values =
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
    Plot.colorbar ?color ?contour ~orient:(`top (0.0375, 0.875)) ~label:[`bottom "Magnitude"] ~pos shade;
  ];

  (* Reset text and tick sizes *)
  plschr 0.0 1.0;
  plsmaj 0.0 1.0;
  plsmin 0.0 1.0

let pi = atan 1.0 *. 4.0

(* Fundamental settings.  See notes[] for more info. *)

let ns = 20             (* Default number of shade levels *)

(* polar plot data *)
let perimeterpts = 100

(*--------------------------------------------------------------------------*\
 * f2mnmx
 *
 * Returns min & max of input 2d array.
\*--------------------------------------------------------------------------*)
let f2mnmx f =
  let fmax = ref f.(0).(0) in
  let fmin = ref f.(0).(0) in
  for i = 0 to Array.length f - 1 do
    for j = 0 to Array.length f.(i) - 1 do
      fmax := max !fmax f.(i).(j);
      fmin := min !fmin f.(i).(j);
    done
  done;
  !fmin, !fmax

module Make(F:Feature.S) =
struct
module F_compare = Data_sort.Feature_compare(F)
let create
    ?dist
    ?inc
    ?(device="png")
    ?title
    ~(feature1:F.t)
    ~(feature2:F.t)
    ~(output:F.t)
    ?(stddev:F.t option)
    ~(sampler:Sampler.t) =
  let fill_width = 2.0 in
  let cont_color = 0 in
  let cont_width = 0.0 in

  (* Parse and process command line arguments *)
  plparseopts Sys.argv [PL_PARSE_FULL];

  (* Load color palettes *)
  plspal0 "cmap0_black_on_white.pal";
  plspal1 "cmap1_blue_yellow.pal" true;

  (* Reduce colors in cmap 0 so that cmap 1 is useful on a 16-color display *)
  plscmap0n 3;

  plsdev device;
  (* Initialize plplot *)
  plinit ();
  let raw_data : float array array = sampler
    |> Data_sort.sort_floats |> Gen.to_array in
  let open Interpolater in
  let {z;_} = with_inc ~x_col:(F.to_int feature1) ~y_col:(F.to_int feature2) ~z_col:(F.to_int output) 
    ?dist ?inc ~data:raw_data in
  (*let xyz_data : float array array = Gen.map (F.sub_array [feature1;feature2;output]) sampler*)
(*    |> Data_sort.sort_floats |> Gen.to_array in *)
  let zmin, zmax = f2mnmx z in
  let shedge =
    Array.init (ns + 1) (
      fun i ->
        zmin +. (zmax -. zmin) *. float_of_int i /. float_of_int ns
    )
  in


  (* Plot using identity transform *)
  pladv 0;
  plvpor 0.1 0.9 0.1 0.9;
  plwind (-1.0) 1.0 (-1.0) 1.0;

  plpsty 0;

  plshades z (-1.0) 1.0 (-1.0) 1.0 shedge fill_width cont_color cont_width true;

  colorbar shedge;

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
