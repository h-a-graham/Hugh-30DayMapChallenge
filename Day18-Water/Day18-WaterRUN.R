
source('Day18-Water/Day18-Water.R')
# THIS IS SLOW - YOU'VE BEEN WARNED!
tic()
animate_maps(out_path='exports/Day18-WaterSpin.mp4',
             rev(seq(0, 359, by=1)), .res=4e3,
             ncores=5)
toc()

tic()
plot_water(out_path='exports/Day18-Water_EU.png', .res=4e3,
           .long = 15, .lat=50,
           scico.col = "lapaz", .direction = 1)
toc()

