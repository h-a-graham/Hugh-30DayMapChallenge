library(raytrix)
library(rayshader)
library(scico)
library(sf)
library(osmextract)
library(dplyr)
library(cowplot)
library(rnaturalearth)
library(tmap)

# set raytrix canvas
set_canvas_centroid(52.98730614503858, 158.58338737882468,
                    radius=70000)

#download buildings
region <-st_bbox(canvasExent()) %>%
  st_as_sfc() %>%
  st_set_crs(3857)
options(timeout = 600)
osm_poly <- oe_get(region, layer='multipolygons',
                   max_file_size = 5e+010, download_only=F,
                   boundary=region, provider = 'openstreetmap_fr',
                   query = "SELECT * FROM 'multipolygons' WHERE building IS NOT NULL")%>%
  st_transform(., crs=3857)

# download coastlines
countires <- ne_coastline(scale='medium',returnclass='sf') %>%
  st_make_valid() %>%
  st_transform(crs='+proj=laea +lon_0=150 +lat_0=30')


# get elevation

topo_mat <-raytrix::topo_matrix(res=50, src='srtm_gl1')

#rayshade
pal.cols <- scico(5, palette = 'lapaz')

ray_tex <- topo_mat %>%
  sphere_shade(texture=create_texture( pal.cols[1], pal.cols[2], pal.cols[3],
                                       pal.cols[4], pal.cols[5],)) %>%
  add_shadow(texture_shade(topo_mat, detail=0.6, contrast = 3, brightness = 6),0.2) %>%
  add_shadow(ray_shade(topo_mat,sunangle = 85, sunaltitude = 35, zscale = 10, multicore = T),0.1) %>%
  add_overlay(generate_waterline_overlay(topo_mat, min = 0.0002, max = 0.1, smooth=2))

# plot_map(ray_tex)

tex_rast <- texture_to_brick(ray_tex) # convert to raster

#plot with tmap
main <- tm_shape(tex_rast, raster.downsample = F)+
  tm_rgb(legend.show = FALSE) +
  # tm_scale_bar(breaks =c(0, 5),color.dark = NA, color.light = NA,text.color = 'white',
  #              text.size = 1.5, lwd=0.01, just='left')+
  tm_shape(st_buffer(osm_poly, 50)) +
  tm_fill(col='#E16121', aplah=0.7)+
  tm_layout(
    title= 'Petropavlovsk-Kamchatsky',
    title.position = c(0.01, 0.96),
    title.color = 'white',
    title.fontfamily = 'Gugi',
    title.size = 20) +
  tm_credits("#30DayMapChallenge   @hughagraham   Made with: Natural Earth, SRTM", position=c("left", "bottom"),
             size=120.0,  col = 'white',fontfamily = 'Gugi')
# main

inset<- tm_shape(countires) +
  tm_lines(col='white', alpha=0.3) +
  tm_layout (frame = FALSE, bg.color = "transparent") +
  tm_shape(st_difference(st_buffer(region, 8e5), st_buffer(region, 10e4))) +
  tm_polygons(col='white', alpha=0.8)


#make grobs
m <- tmap_grob(main)
i <- tmap_grob(inset)

#draw and save
im <- ggdraw() +
  draw_plot(m) +
  draw_plot(i,
            width = 0.4, height = 0.4,
            x = 0.58, y = 0.025)

cowplot::save_plot('exports/Day16-UrbanRural.jpg', im, base_width=10, base_height=10)

