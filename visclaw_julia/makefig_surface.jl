# ============================================================================================
# (program) makefig_surface
# Reference: https://github.com/hydrocoast/VisClaw.jl/blob/master/Examples_using_Plots.ipynb
# Description; plot sea surface to check output by GeoClaw
# ============================================================================================

using VisClaw
using Plots

# chile2010 _output
simdir = "D://Research//GeoClaw//WRFClaw//WRFClaw_CalSet//dev//_output"


# load surface (fort.q0000 to fort.q0018)
amrall = loadsurface(simdir)
rmvalue_coarser!.(amrall.amr)

# check
println(typeof(amrall))
println(fieldnames(typeof(amrall)))
amrall.nstep, amrall.timelap

mycolor = Plots.cgrad([:blue, :white, :red])

# plot
plts = plotsamr(amrall; c=mycolor, clims=(-0.5,0.5))
plot(plts[6])

# overlay the grid numbers, and the tiles
plts = gridnumber!.(plts, amrall.amr; font=Plots.font(12, :black, :center))
plts = tilebound!.(plts, amrall.amr)

# make an animation
anim = Plots.Animation()
map(p->Plots.frame(anim, p), plts)
Plots.gif(anim, "flatplane_eta.gif"; fps=3)

# show a snapshot
plot(plts[5])