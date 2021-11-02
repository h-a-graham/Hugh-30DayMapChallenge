#Day1: Points
# Global elevation map.

#---- load libs ------
library(showtext)
library(dplyr)
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
topo_points <- gdalio_xyz(topography::topography_source("aws"),
                          resample='Cubic') %>%
  as.data.frame() %>%
  filter(Band1 != 0)

# rounding function for legend
round_nearest <- function(.x, .f, .round=100){
  round_any(.f(.x$Band1, na.rm=T), .round)
}

minmax <- function(.x, .colname){
  .x %>%
    dplyr::summarise(.min=min({{.colname}}),
              .max=max({{.colname}})) %>%
    slice(1) %>%
    unlist(., use.names=FALSE)
}

showtext_auto() # to allow text plotting

# reate plot with ggplot2
p <- ggplot(topo_points, aes(x=x, y=y,colour=Band1)) +
  geom_point(size=0.5) +
  scale_colour_gradientn(colours=scico(n=255, palette = 'vikO'),
                         breaks=c(round_nearest(topo_points, min),
                                  round_nearest(topo_points, max)),
                         limits=c(round_nearest(topo_points, min),
                                  round_nearest(topo_points, max)))+
  labs(colour='', title='Elevation (m.s.l)') +
  coord_fixed(xlim=minmax(topo_points, x),
                  ylim=minmax(topo_points, y)) +
  theme_void() +
  theme(legend.position = c(0.475, -0.1),
        legend.direction="horizontal",
        plot.margin=unit(c(0.1,0.1,2,0.1),"cm"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(family='Poiret One', face='bold', size=45, colour='white'),
        legend.text = element_text(family='Poiret One', face='bold', size=35, colour='white')) #
# p

#save the plot
ggsave(filename = 'exports/PointsWorldElevation_vikO2.jpg', p)



