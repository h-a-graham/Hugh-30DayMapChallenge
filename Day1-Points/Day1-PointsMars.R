#Day1: Points
# Mars elevation map.

#---- load libs ------
library(showtext)
library(dplyr)
library(gdalio)
library(topography)
library(ggplot2)
library(plyr)

source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))
# ---- import fonts ----------
# Add fonts if needed
#
font_add_google(name = "Poiret One", family = "Poiret One")

# for mars??
gdalio_set_default_grid(list(extent = c(-180, 180, -85, 85),
                             dimension = as.integer(c(360*0.8, 170*0.8)),
                             projection = "+proj=longlat +R=3389500"))

topo_points <- gdalio_xyz(topography::topography_source("mars"),
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
  geom_point(size=0.05) +
  scale_colour_viridis_c(option='turbo',
                         breaks=c(round_nearest(topo_points, min),
                                  round_nearest(topo_points, max)),
                         limits=c(round_nearest(topo_points, min),
                                  round_nearest(topo_points, max)))+
  labs(colour='', title='Elevation of Mars') +
  coord_fixed(xlim=minmax(topo_points, x),
              ylim=minmax(topo_points, y)) +
  theme_void() +
  theme(legend.position = c(0.475, -0.1),
        legend.direction="horizontal",
        plot.margin=unit(c(0.1,0.1,2,0.1),"cm"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(family='Poiret One', face='bold', size=45, colour='white'),
        legend.text = element_text(family='Poiret One', face='bold', size=35, colour='white'))
# p
#save the plot
ggsave(filename = 'exports/MarsElevation_Turbo.jpg', p)




