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
let nx = 35             (* Default number of data points in x *)
let ny = 46             (* Default number of data points in y *)
let exclude = false     (* By default do not plot a page illustrating
                           exclusion. *)

(* polar plot data *)
let perimeterpts = 100

(* Transformation function *)
let mypltr x y tr =
  tr.(0) *. x +. tr.(1) *. y +. tr.(2),
  tr.(3) *. x +. tr.(4) *. y +. tr.(5)

let zdefined x y =
  let z = sqrt (x *. x +. y *. y) in
  if z < 0.4 || z > 0.6 then 1 else 0

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

(*--------------------------------------------------------------------------*\
 * Does several shade plots using different coordinate mappings.
\*--------------------------------------------------------------------------*)
module Make(F:Feature.S) =
struct
let create
    ?(device="png")
    ?title
    ~(feature1:F.t)
    ~(feature2:F.t)
    ~(output:F.t)
    ~(data:Sampler.t) =
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


  (* Set up data arrays *)
  let z = Array.make_matrix nx ny 0.0 in
  let w = Array.make_matrix nx ny 0.0 in
  for i = 0 to nx - 1 do
    let x = float_of_int (i - (nx / 2)) /. float_of_int (nx / 2) in
    for j = 0 to ny - 1 do
      let y = float_of_int (j - (ny / 2)) /. float_of_int (ny / 2) -. 1.0 in
      z.(i).(j) <- -. sin (7.0 *. x) *. cos (7.0 *. y) +. x *. x -. y *. y;
      w.(i).(j) <- -. cos (7.0 *. x) *. sin (7.0 *. y) +. 2.0 *. x *. y;
    done
  done;

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
