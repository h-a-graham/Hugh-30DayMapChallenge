source('Day17-Land/Day17-Land.R')

# THIS IS SLOW - YOU'VE BEEN WARNED!
tic()
animate_maps(out_path='exports/Day17-LandSpin.mp4',
             rev(seq(0, 359, by=1)), .res=4e3,
             ncores=5)
toc()

tic()
plot_land(out_path='exports/Day17-Land_NZ..png', .res=4e3, .long = 150, .lat=-30)
toc()
