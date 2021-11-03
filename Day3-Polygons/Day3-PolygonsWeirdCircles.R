library(gdalio)
library(dplyr)
library(topography)
library(ggplot2)
library(showtext)
library(sf)
library(sfheaders)
library(scico)
source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))

font_add_google(name = "Major Mono Display", family = "Major Mono Display")
# get data with gdalio


gdalio_set_default_grid(list(extent=c(165.305211, 179.385818, -47.315717, -33.675928),
                             projection="+proj=longlat +datum=WGS84",
                             dimension=c(x=100, y=100)))

topo_points <- gdalio_xyz(topography::topography_source("aws"),
                          resample='Cubic') %>%
  as.data.frame() %>%
  filter(Band1 > 0) %>%
  bind_cols(sf_point(., "x", "y")) %>%
  st_as_sf(crs="+proj=longlat +datum=WGS84") %>%
  st_transform(crs=3857) %>%
  mutate(p_alpha = Band1/max(Band1),
         flippedZ= max(Band1) * (1 - (Band1/max(Band1)))) %>%
  arrange(desc(flippedZ)) %>%
  st_buffer(., .$flippedZ*50)

showtext_auto()

p <- ggplot(topo_points, aes(fill=Band1)) +
  geom_sf(colour=alpha('grey90',0.1)) +
  scale_fill_gradientn(colours=scico(n=255, palette = 'tokyo'),
                       limits=c(0, 1500), breaks=c(0, 1500)) +
  labs(fill='', size='', alpha='', title='New Zealand',
       subtitle = 'Elevation (m.s.l)',
       caption ='#30DayMapChallenge')+
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        legend.direction="horizontal",
        title = element_text(family='Major Mono Display', face='bold', size=75, colour='white'),
        plot.title = element_text(vjust = -100, hjust = 0.02),
        plot.subtitle = element_text(vjust = -220, hjust = 0.02,size=45),
        plot.caption = element_text(size=35),
        legend.text = element_text(family='Major Mono Display', face='bold', size=55, colour='white'),
        panel.background = element_rect(fill = 'black', colour = 'black'))

ggsave(filename = 'exports/NZ_bufferedElevation.jpg', p, dpi=600)


