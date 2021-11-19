#moochrome - Dartmoor's Tors
library(sf)
library(plyr)
library(dplyr)
library(rayshader)
library(topography)
library(raster)
library(stars)
library(rmapshaper)
library(gdalio)
library(stringr)
library(roughsf)

source(system.file("raster_format/raster_format.codeR",
                   package = "gdalio", mustWork = TRUE))

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
#from here: https://naturalengland-defra.opendata.arcgis.com/datasets/national-parks-england/explore?location=52.206329%2C-1.005742%2C7.53
dartmoor <- read_sf('data/National_Parks_(England)/National_Parks__England____Natural_England.shp') %>%
  filter(NAME=='DARTMOOR')

dartmoorMask <- st_buffer(dartmoor, 100000) %>%
  st_difference(dartmoor) %>%
  st_as_sf()

tors <- read_sf('data/opname_gpkg_gb/data/opname_gb.gpkg',
                       query = "SELECT NAME1, LOCAL_TYPE, geom  FROM \'NamedPlace'\ WHERE LOCAL_TYPE == 'Hill Or Mountain'") %>%
  filter(st_intersects(., dartmoor, sparse = FALSE)[,1]) %>%
  filter(str_detect(NAME1, "Tor"))


#get raster data and shade
.res=100
set_gdalio_sf(st_buffer(dartmoor, 5000), .res)

ras <- gdalio_raster(dsn=topography::topography_source('aws'),
                    resample='cubic')

elmat <- raster_to_matrix(ras)

texture <- elmat %>%
  ambient_shade(sunbreaks = 24,maxsearch = 30, multicore=TRUE)

# plot_map(texture)

#convert texture to raster
g <- gdalio_get_default_grid()

rotatef = function(x) t(apply(x, 2, rev))
newarrayt <- rotatef(texture)

tex_r <- raster::raster(scales::rescale(newarrayt, to = c(0, 255)),
                       xmn = g$extent[1],
                       xmx = g$extent[2],
                       ymn = g$extent[3],
                       ymx = g$extent[4],
                       crs = g$projection)

cuts<-10
tex_cut <- cut(tex_r, cuts)

# plot(tex_cut)
# Convert raster to polygons...
tex_s <- stars::st_as_stars(tex_cut)

tex_sf <- st_as_sf(tex_s, merge=T) %>%
  ms_simplify() %>%
  st_cast(., "POLYGON")

tex_sf2 <- tex_sf %>%
  mutate(fill = '#00000',
         color = '#00000',
         stroke = layer*0.01,
         fillweight = scales::rescale(layer, c(0,1))*0.1+0.001,
         fillstyle = sample(c("hachure", "solid", "zigzag", "cross-hatch"),1)) %>%
  st_cast(., "POLYGON") %>%
  filter(st_intersects(., dartmoor, sparse = FALSE)[,1])

tors_2 <- tors %>%
  mutate(size=5,
         color="#606060",
         label_pos = "e")

tors_3 <- tors %>%
  filter(NAME1 %in% c('Hanging Stone Tor', 'Belstone Tor',
                      'Sheeps Tor', 'Flat Tor', 'Hound Tor',
                      'Crockern Tor', 'Brent Tor', "Hunter's Tor")) %>%
  mutate(size=8,
         color="#606060",
         label = NAME1,
         label_pos = "e")

p <- roughsf::roughsf(list(tex_sf2, tors_2, tors_3),
                 title = "Dartmoor's Tors", caption = "#30DayMapChallenge   @hughagraham Contains OS data © Crown copyright and database right (2021)",
                 title_font = "48px Megrim", font = "30px Megrim", caption_font = "15px Megrim",
                 roughness = 1, bowing = 1, simplification = 1,
                 width = 1000, height = 1000)

save_roughsf(p, 'exports/Day9-MonochromeSKETCH.png')
