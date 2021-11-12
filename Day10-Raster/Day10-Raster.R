# Day 10 Raster
library(EAlidaR)
library(rayshader)
library(raytrix)
library(tmap)
library(tidyverse)
library(sf)
library(raster)
library(showtext)
library(rnaturalearth)
library(cowplot)

# ---- Download Data -----
# Get lidar
if (!dir.exists('myCache')) dir.create('myCache')
if (!file.exists('myCache/IoS.tif')){
  IoS <- get_from_xy(xy=c(090034 , 012069), radius=8000, resolution = 1,
                     model_type = 'DSM', dest_folder='myCache',out_name= 'IoS.tif')
  IoS <- raster(IoS)
} else {
  IoS <- raster('myCache/IoS.tif')
}
crs(IoS) <- st_crs(27700)$wkt

# get gb outline.
GB <- ne_countries(country = 'united kingdom', scale='large', returnclass='sf') %>%
  st_transform(crs=st_crs(tex_r))

# --- process raster data ------
IoS[IoS<0] <- 0
IoS[is.na(IoS)] <- 0
# plot(IoS)

IoS_mat <- raster_to_matrix(IoS) %>%
  resize_matrix(., 0.25)

# set plotting canvas with raytrix
set_canvas_raster(IoS)
#download overlay with raytrix
ov <- map_drape(res=5, src="wms_arcgis_mapserver_ESRI.WorldImagery_tms",
                alpha = 0.7)

#  ----- rayshade ------
IoS_Map <- IoS_mat %>%
  sphere_shade(texture='imhof4')%>%
  add_overlay(., ov,rescale_original=T) %>%
  add_shadow(texture_shade(IoS_mat, detail=0.6, contrast = 3, brightness = 6),0.2) %>%
  add_shadow(ray_shade(IoS_mat, sunaltitude = 35, zscale = 3, multicore = T),0.1) %>%
  add_overlay(generate_waterline_overlay(IoS_mat, min = 0.0002, max = 0.1, smooth=2))

# save_png(IoS_Map, filename = 'ideas/IoSTEST2.png')
# plot_3d(IoS_Map, IoS_matRes, zscale=100)

# convert rayshade texture to raster.
g <- get_canvas()
tex_r <- raster::brick(scales::rescale(IoS_Map, to = c(0, 255)),
                       xmn = g$extent[1],
                       xmx = g$extent[2],
                       ymn = g$extent[3],
                       ymx = g$extent[4],
                       crs = g$projection)

# ----- Plotting -----
# Get fonts...
font_add_google(name = "Lobster", family = "Lobster")
showtext_auto()

# plot with tmap
main <- tm_shape(tex_r, raster.downsample = F)+
  tm_rgb(legend.show = FALSE) +
  tm_scale_bar(breaks =c(0, 5),color.dark = NA, color.light = NA,text.color = 'white',
               text.size = 1.5, lwd=0.01, just='left')+
  tm_layout(
    attr.position = c(0.01, 0.88),
    title= 'Isles of Scilly',
    title.position = c(0.065, 0.96),
    title.color = 'white',
    title.fontfamily = 'Lobster',
    title.size = 7) +
  tm_credits("#30DayMapChallenge   @hughagraham   Made with Natural Earth; @EnvAgencyGeomat; ESRI world imagery", position=c("left", "bottom"),
             size=120.0,  col = 'white')
# main
# tmap_save(main, "ideas/scillytmap.png")

inset<- tm_shape(GB) +
  tm_borders(col='white', alpha=0.3) +
  tm_layout (frame = FALSE, bg.color = "transparent") +
  tm_shape(st_bbox(tex_r) %>% st_as_sfc() %>% st_buffer(10000)) +
  tm_fill(col='white', alpha=0.8)+
  tm_shape(st_bbox(tex_r) %>% st_as_sfc()) +
  tm_borders(col='black')

#make grobs
m <- tmap_grob(main)
i <- tmap_grob(inset)

#draw and save
im <- ggdraw() +
  draw_plot(m) +
  draw_plot(i,
            width = 0.4, height = 0.4,
            x = 0.58, y = 0.025)

cowplot::save_plot('exports/Day10-Raster.jpg', im, base_width=6, base_height=6)


