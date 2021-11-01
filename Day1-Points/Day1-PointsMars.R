#Day1: Points
# Global elevation map.



#---- load libs ------
library(showtext)
library(stars)
library(sf)
library(gdalio)
library(topography)
library(ggplot2)
library(plyr)
# library(scico)

source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))
# ---- import fonts ----------
# Add fonts if needed
#
font_add_google(name = "Poiret One", family = "Poiret One")

# for mars??
gdalio_set_default_grid(list(extent = c(-180, 180, -85, 85),
                             dimension = as.integer(c(360*0.8, 170*0.8)),
                             projection = "+proj=longlat +R=3389500"))

# gdalio_set_default_grid(list(extent=c(-1,1,-1,1)*5e7,
#                              projection='+proj=longlat +a=3396190 +b=3376200 +no_defs ',
#                              dimension=c(x=100, y=100)))

topo <- gdalio_stars(topography::topography_source("mars"),
                     resample='Cubic')
# plot(topo)
c(50, 180, -85, -20)
# remove No data
topo[topo==0] <- NA

# convert to points
topo_points <- st_xy2sfc(topo, as_points = T, na.rm=T) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.))

plot(topo_points, cex=0.1)

p <- ggplot(topo_points, aes(x=X, y=Y,colour=values)) +
  geom_point(size=0.001) +
  scale_colour_viridis_c(option='turbo',
                         breaks=c(round_nearest(topo, min),
                                  round_nearest(topo, max)),
                         limits=c(round_nearest(topo, min),
                                  round_nearest(topo, max)))+
  labs(colour='', title='Elevation of Mars') +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        title = element_text(family='Poiret One', face='bold', size=45, colour='white'),
        legend.text = element_text(family='Poiret One', face='bold', size=35, colour='white')) #

p
#save the plot
ggsave(filename = 'exports/MarsElevation_Turbo.jpg', p)




