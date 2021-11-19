#Islands - Corse
library(sf)
library(raytrix)
library(rayshader)
library(raster)
library(tmap)
library(rnaturalearth)
library(cowplot)

corse_proj <- '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
Corsica  <- getData("GADM",country="France",level=2, path='myCache') %>%
  st_as_sf() %>%
  dplyr::filter(NAME_2 %in% c("Corse-du-Sud", "Haute-Corse")) %>%
  st_union() %>%
  st_transform(crs=corse_proj) %>%
  st_buffer(5000)

countires <- ne_coastline(scale='medium',returnclass='sf') %>%
  st_make_valid() %>%
  st_transform(crs='+proj=laea +lon_0=0 +lat_0=20') %>%
  st_intersection(st_buffer(Corsica, 4e6) %>%st_transform('+proj=laea +lon_0=0 +lat_0=20'))

set_canvas_sf(Corsica)
topMat <- topo_matrix(res=50)
satArr <- map_drape(res=25)

texture <- satArr %>%
  add_shadow(ray_shade(topMat, sunaltitude = 20, sunangle = 200, zscale=35,
                       multicore=TRUE),0.2)

tex_rast <- texture_to_brick(texture)

#plot with tmap
main <- tm_shape(tex_rast, raster.downsample = F)+
  tm_rgb(legend.show = FALSE) +
  tm_layout(
    title= 'Corse',
    title.position = c(0.12, 0.96),
    title.color = 'white',
    title.fontfamily = 'Tangerine',
    title.size = 60) +
  tm_credits("#30DayMapChallenge   @hughagraham   Made with: Natural Earth, SRTM",
             position=c("left", "bottom"),
             size=220.0,  col = 'white',fontfamily = 'Tangerine')
# main

ext_box <- st_bbox(canvasExent()) %>%
  st_as_sfc() %>%
  st_set_crs(corse_proj)

inset<- tm_shape(countires) +
  tm_lines(col='white', alpha=0.3) +
  tm_layout (frame = FALSE, bg.color = "transparent") +
  tm_shape(st_buffer(ext_box, 1e5, joinStyle='BEVEL')) +
  tm_borders(col='white', alpha=0.8, lwd=2)


#make grobs
m <- tmap_grob(main)
i <- tmap_grob(inset)

#draw and save
im <- ggdraw() +
  draw_plot(m) +
  draw_plot(i,
            width = 0.2, height = 0.2,
            x = 0.28, y = 0.72)

cowplot::save_plot('exports/Day19-Islands.png', im, base_height = 10, base_asp=1)
