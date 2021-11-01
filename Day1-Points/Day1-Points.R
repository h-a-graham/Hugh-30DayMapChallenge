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
library(scico)

source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))

# ---- import fonts ----------
# Add fonts if needed
#
font_add_google(name = "Poiret One", family = "Poiret One")

#--- make a map ----------
# get data with gdalio
gdalio_set_default_grid(list(extent=c(-1,1,-1,1)*1.4e7,
                             projection='+proj=laea',
                             dimension=c(x=100, y=100)))

topo <- gdalio_stars(topography::topography_source("aws"),
                     resample='Cubic')

# remove No data
topo[topo==0] <- NA

# convert to points
topo_points <- st_xy2sfc(topo, as_points = T, na.rm=T) %>%
  st_as_sf()

# rounding function for legend
round_nearest <- function(.stars, .f, .round=100){
  round_any(.f(.stars$values, na.rm=T), .round)
}

showtext_auto() # to allow text plotting

# reate plot with ggplot2
p <- ggplot(topo_points, aes(colour=values)) +
  geom_sf(size=0.5) +
  scale_colour_gradientn(colours=scico(n=255, palette = 'vikO'),
                         breaks=c(round_nearest(topo, min),
                                  round_nearest(topo, max)),
                         limits=c(round_nearest(topo, min),
                                  round_nearest(topo, max)))+
  labs(colour='', title='Elevation (m.s.l)') +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        title = element_text(family='Poiret One', face='bold', size=45, colour='white'),
        legend.text = element_text(family='Poiret One', face='bold', size=35, colour='white')) #
# p

#save the plot
ggsave(filename = 'exports/PointsWorldElevation_vikO.jpg', p)



