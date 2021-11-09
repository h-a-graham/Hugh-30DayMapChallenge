#moochrome
library(sf)
library(dplyr)
library(plyr)
library(rayshader)
library(topography)
library(raster)
library(gdalio)
library(tmap)
library(showtext)
library(rnaturalearth)
library(cowplot)
source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))

#functions
# set gdalio extent
set_gdalio_sf <- function(aoi, res){
  bounds <- aoi %>%
    st_bbox()
  grid0 <- list(extent = round_any(c(bounds$xmin, bounds$xmax, bounds$ymin, bounds$ymax),10),
                dimension = round_any(c((bounds$xmax-bounds$xmin)/res,
                                        (bounds$ymax-bounds$ymin)/res),10),
                projection = st_crs(aoi)$wkt)
  gdalio_set_default_grid(grid0)
}

mono_pal <- function(mono.col='#000000', n=255){
  pal <-colorRampPalette(c(mono.col,'#FFFFFF'))
  return(pal(n))
}


# Process sf data
dartmoor <- read_sf('data/National_Parks_(England)/National_Parks__England____Natural_England.shp') %>%
  filter(NAME=='DARTMOOR')

dartmoorMask <- st_buffer(dartmoor, 100000) %>%
  st_difference(dartmoor) %>%
  st_as_sf()


#get raster data and shade
.res=10
set_gdalio_sf(st_buffer(dartmoor, 5000), .res)

ras <- gdalio_raster(dsn=topography::topography_source('aws'),
                    resample='cubic')

elmat <- raster_to_matrix(ras)


texture <- elmat %>%
  height_shade(texture=mono_pal()) %>%
  add_shadow(ray_shade(elmat, maxsearch = 100, multicore = TRUE, zscale=.res*0.75),0.3) %>%
    add_shadow(texture_shade(elmat, detail=0.5, contrast=2, brightness = 3),0.4)


#convert texture to raster
g <- gdalio_get_default_grid()
tex_r <- raster::brick(scales::rescale(texture, to = c(0, 255)),
                       xmn = g$extent[1],
                       xmx = g$extent[2],
                       ymn = g$extent[3],
                       ymx = g$extent[4],
                       crs = g$projection)

# Get fonts...
font_add_google(name = "Megrim", family = "Megrim")
showtext_auto()

main <- tm_shape(tex_r, raster.downsample = F)+
  tm_rgb(legend.show = FALSE) +
  tm_shape(dartmoorMask) +
  tm_polygons(col='black', alpha=0.5) +
  tm_scale_bar(breaks =c(0, 12),color.dark = "white", text.color = "white",
               text.size = 1.5, lwd=2, just='left')+
  tm_layout(
            attr.position = c(0.005, 0.83),
            title= 'Dartmoor',
            title.position = c(0.01, 0.975),
            title.color = 'white',
            title.fontfamily = 'Megrim',
            title.size = 7)

GB <- ne_countries(country = 'united kingdom', scale='large', returnclass='sf') %>%
  st_transform(crs=st_crs(dartmoor))

inset<- tm_shape(GB) +
  tm_borders(col='white') +
  tm_layout (frame = FALSE, bg.color = "transparent") +
  tm_shape(st_bbox(tex_r) %>% st_as_sfc() %>% st_buffer(5000)) +
  tm_fill(col='white', alpha=0.9)+
  tm_shape(st_bbox(tex_r) %>% st_as_sfc()) +
  tm_borders(col='black')


m <- tmap_grob(main)
i <- tmap_grob(inset)


im <- ggdraw() +
  draw_plot(m) +
  draw_plot(i,
            width = 0.4, height = 0.4,
            x = 0.58, y = 0.025)
iml <-  add_sub(im, label="#30DayMapChallenge   @hughagraham   Made with Natural Earth")

cowplot::save_plot('exports/Day9-Monochrome.jpg', im, base_width=6, base_height=6)


