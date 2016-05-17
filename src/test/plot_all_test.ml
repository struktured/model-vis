module G = Gaussian_plot

let gauss_plots () =
  G.surface_plot ();
  G.surface_3d_plot ();
  G.line_plot ()

let () =
begin
  gauss_plots()
end



