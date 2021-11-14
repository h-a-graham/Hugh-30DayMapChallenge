library(EAlidaR)
library(rayshader)
library(raster)
library(viridisLite)
library(furrr)
library(purrr)

# ---- Functions ----
read_lidar <- function(.xy){
  r <- get_from_xy(xy=.xy, radius=1500, resolution = 1, model_type = 'DTM')
  raster_to_matrix(raster(r))
}

biasTurbo <- function(bias, n, direction){
  pal <- colorRampPalette(turbo(n, direction=direction), bias=bias)
  return(pal(n))
}

estuary_texture <- function(elmat, bias, n=256, direction=1){
  elmat %>%
    height_shade(texture=biasTurbo(bias, n, direction)) %>%
    add_shadow(texture_shade(elmat, detail=0.5, contrast=8, brightness = 6),0) %>%
    add_shadow(lamb_shade(elmat, sunaltitude = 20, zscale = 0.75),0)
}

# ---- Map ----

estuary_coords <- list(c(581181 , 169832), c(604234 , 217959), c(326660 , 374961))

plan(multisession, workers = 3)
# lidar_matrices <-readRDS('myCache/rasterList.rds')
lidar_matrices <- future_map(estuary_coords, ~ read_lidar(.x))
saveRDS(lidar_matrices, 'myCache/rasterList.rds')
colour_bias <- c(10, 3, 2.5)
direction <- c(1, -1, -1)

textures <- purrr::pmap(list(lidar_matrices, colour_bias, direction),
                 ~ estuary_texture(..1, bias=..2, direction = ..3))
#indiviual maps
# iwalk(textures, ~ save_png(.x, filename = sprintf('exports/EstuaryTestuary%s.png', .y)))

#panel map
png(filename = 'exports/Day678-RGBp.png', height=1440*2, width=480*2)
par(mfrow=c(3,1))
walk2(textures, c('R', 'G', 'B'),
      ~ plot_map(.x, title_text = .y,
                 title_font='Megrim',
                 title_style='bold',
                 title_size=500,
                 title_color = "#D5D5D5" ))
text(2.1e3, 0.05e3, "#30DayMapChallenge   @hughagraham   Data from: @EnvAgencyGeomat",
     cex=1.8, col='#D5D5D5')
dev.off()



