open Plplot

let init () = plinit ()
let coords f  = f 0.0 0.0 1.0 1.0
let omega_vpor () = coords plvpor
let omega_env () = coords plenv 0 1
let omega_plot_init () = Plot.init (0.0, 0.0) (1.0, 1.0) `equal `prompt
