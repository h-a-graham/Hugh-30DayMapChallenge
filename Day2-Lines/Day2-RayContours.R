library(gdalio)
library(dplyr)
library(topography)
library(ggplot2)
library(showtext)
library(sf)
library(metR)
library(patchwork)
library(cowplot)
source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))

#add fonts
font_add_google(name = "Allerta Stencil", family = "Allerta Stencil")

# Functions...
#sets gdalio grid
set_canvas_sf <- function(.sf, .dim){

  bounds <- .sf %>%
    sf::st_bbox()
  canvas0 <- list(extent = c(bounds$xmin, bounds$xmax, bounds$ymin, bounds$ymax),
                  projection = sf::st_crs(.sf)$wkt,
                  dimension=c(.dim, .dim))

  gdalio::gdalio_set_default_grid(canvas0)
}
#gets extent from lat long and radius
set_grid_centroid <- function(lat, long, .dim, .radius=5000,
                                projection="+proj=longlat +datum=WGS84"){

  extent_sfc <- sf::st_sfc(sf::st_point(c(long, lat)))%>%
    sf::st_set_crs(projection)

  extent_sfc <- extent_sfc %>%
      sf::st_transform(crs=3857)

  extent_sfc <- extent_sfc %>%
    sf::st_buffer(.radius)%>%
    sf::st_bbox()%>%
    sf::st_as_sfc()

  set_canvas_sf(extent_sfc, .dim)
}
# bound setting function for plot
minmax <- function(.x, .colname){
  .x %>%
    dplyr::summarise(.min=min({{.colname}}),
                     .max=max({{.colname}})) %>%
    slice(1) %>%
    unlist(., use.names=FALSE)
}


contour_plot <- function(.lat, .long,
                         .title, .bg_col, .lin_col, .tit_col,
                         .dim=1000, .radius=2000){

  set_grid_centroid(.lat, .long, .dim, .radius = .radius)


  topo_points <- gdalio_xyz(topography::topography_source("aws"),
                            resample='Cubic') %>%
    as.data.frame()

  showtext_auto()
  ggplot(topo_points, aes(x=x, y=y, z=Band1)) +
    geom_contour(colour=.lin_col, bins=20) +
    geom_text_contour(aes(z = Band1),stroke = 0.2,
                      label.placer = label_placer_flattest(),
                      stroke.colour=.bg_col, color=.lin_col,
                      family='Allerta Stencil',min.size = 5, skip=1,
                      check_overlap=T) +
    coord_fixed(xlim=minmax(topo_points, x),
                ylim=minmax(topo_points, y)) +
    theme_nothing() +
    labs(title=.title)+
    guides(colour="none") +
    theme(panel.background = element_rect(fill = .bg_col, colour = .bg_col),
          plot.title = element_text(vjust = -3.5, hjust = 0.02),
          # plot.margin = unit(c(0,0,0,0),"cm"),
          title = element_text(family='Allerta Stencil', face='bold', size=35, colour=.tit_col))

}


p1 <- contour_plot(3.169269489632889, 98.3926306363336,
             .title='Mount Sinabung', .bg_col='#23533B',
             .lin_col='#AA4325', .tit_col='#D39527')

p2 <- contour_plot(19.023128391825598, -98.62274759029508,
                   .title='Popocatépetl', .bg_col='#266A85',
                   .lin_col='#9B242A', .tit_col='#F7F7F7' ) #'#F7F7F7'
p3 <- contour_plot(-1.5226811151793127, 29.249456547735342,
                   .title='Mt Nyiragongo', .bg_col='#D39527',
                   .lin_col='#276D87', .tit_col='#9B242A')


png('exports/VolcanoContours.png', height=670, width=1800)
p <- p1 + p2 + p3
plot(p)
dev.off()

