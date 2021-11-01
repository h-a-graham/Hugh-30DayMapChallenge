# Day1-PointsAnimation
#Day1: Points
# Global elevation map.
# This needs work - would be better to rework it with base plot but... time...
#---- load libs ------
library(showtext)
library(stars)
library(sf)
library(gdalio)
library(topography)
library(ggplot2)
library(plyr)
library(scico)
library(furrr)
library(tictoc)

source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))


#--- Functions ----------

# rounding function for legend
round_nearest <- function(.stars, .f, .round=100){
  round_any(.f(.stars$values, na.rm=T), .round)
}

#plotting function
plot_globe <- function(.long, .res, .dir){
  # Add fonts if needed
  font_add_google(name = "Poiret One", family = "Poiret One")

  gdalio_set_default_grid(list(extent=c(-4e6,  4e6, -4e6,  4e6 ),
             projection=sprintf(
               '+proj=tpers +h=5500000 +lat_0=40 +lon_0=%s +azi=-23.5', .long),
             dimension=c(.res, .res)))

  topo <- gdalio_stars(topography::topography_source("aws"),
                       resample='Lanczos')

  # remove No data
  topo[topo==0] <- NA

  # convert to points
  topo_points <- st_xy2sfc(topo, as_points = T, na.rm=T) %>%
    st_as_sf() %>%
    cbind(., st_coordinates(.))



  showtext_auto() # to allow text plotting

  # reate plot with ggplot2
  tryCatch({
    p <- ggplot(topo_points, aes(x=X, y=Y, colour=values)) +
      geom_sf(size=0.5) +
      scale_colour_gradientn(colours=scico(n=255, palette = 'vikO'),
                             breaks=c(-6300,
                                      5400),
                             limits=c(-6300,
                                      5400))+
      labs(colour='', title='Elevation (m.s.l)') +
      theme_void() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            title = element_text(family='Poiret One', face='bold', size=45, colour='white'),
            legend.text = element_text(family='Poiret One', face='bold', size=35, colour='white')) #

    #save the plot
    f_name <- file.path(.dir, sprintf('img_%s.png',.long))
    ggsave(filename = f_name, p)

    return(f_name)
  }, error=function(e){})


}

animate_maps <- function(long_list, .res, dest, temp_dir=tempdir(), fps = 32, ncores=5){
  #set initial canvas
  gdalio_set_default_grid(list(extent=c(-3.8e6,  3.8e6, -3.8e6,  3.8e6 ),
                          projection='+proj=tpers +h=5500000 +lat_0=40 +azi=-23.5',
                          dimension=c(.res, .res)))

  plan(multisession, workers = ncores)
  g <- gdalio_get_default_grid()

  l <- furrr::future_map(.x=long_list, ~plot_globe(.x, .res, temp_dir))

  av::av_encode_video(unlist(l), output = normalizePath(dest), framerate = fps,
                      vfilter = paste0("scale=",g$dimension[1]*10,":-2"))
  unlink(temp_dir)
  future:::ClusterRegistry("stop")
}


tic()
animate_maps(rev(seq(0, 359, by=0.5)), 100, 'exports/pointsAnimation.mov',
             ncores=parallel::detectCores())
toc()
