# imports
library(raytrix)
library(EAlidaR)
library(rayshader)
library(terra)
library(raster)
library(sf)

# Download lidar
if (!file.exists('myCache/CoL_MASK.tif')){
  dsm<- EAlidaR::get_area(city_of_london_sf, resolution = 1, model_type = 'DSM',
                          dest_folder = 'myCache', out_name = 'CoLDSM.tif')
  dtm<- EAlidaR::get_area(city_of_london_sf, resolution = 1, model_type = 'DTM',
                          dest_folder = 'myCache', out_name = 'CoLDTM.tif')
  dsm[is.na(dsm)] <- dtm

  colCROP <- terra::crop(dsm, vect(city_of_london_sf))
  colCROP <- terra::mask(colCROP, vect(city_of_london_sf))
  terra::writeRaster(colCROP, 'myCache/CoL_MASK.tif')
} else {
  colCROP <- terra::rast('myCache/CoL_MASK.tif')
}

# raytrix layers
set_canvas_raster(raster::raster(colCROP))
tc <- raster_to_matrix(raster(colCROP))
ov <- map_drape(0.5, alpha=0.8)

#rayshade texture
tex <- tc %>%
  sphere_shade(texture='imhof4')%>%
  add_overlay(., ov,rescale_original=T) %>%
  add_shadow(texture_shade(tc, detail=0.3, contrast=2, brightness = 3),0) #%>%
add_shadow(ray_shade(tc, sunaltitude = 35, sunangle = 100),0.1)

# 3d plot
plot_3d(tex, tc, solid =F, windowsize = 1200)
render_camera(theta=0, phi=60, zoom=0.6, fov=0)

# Render HQ
cache_f <- 'myCache/ColRayCache'
render_highquality(filename = 'exports/Day11-3DCityOfLondon.png',
                   lightdirection = c(60,110, 240),
                   lightaltitude=c(90,25, 12),
                   lightintensity=c(100, 500, 450),
                   lightcolor = c("white", "#FF9956", "#73A7E1"),
                   # environment_light = 'data/syferfontein_1d_clear_4k.hdr', # download from: https://polyhaven.com/hdris/sunrise-sunset
                   cache_filename=cache_f,
                   samples =200)

render_movie(filename = 'exports/CoL3Dmovie.mp4', type='oscillate')

