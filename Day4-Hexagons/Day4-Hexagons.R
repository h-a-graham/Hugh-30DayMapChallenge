library(gdalio)
library(dplyr)
library(gdalwebsrv)
library(ggplot2)
library(showtext)
library(sf)
library(sfheaders)
library(tictoc)
library(furrr)
source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))



# --- functions --------

# stolen from gdalio :)
.gdalio_grid_coords_cent <- function() {
  g <- gdalio_get_default_grid()
  res <- c(diff(g$extent[1:2])/g$dimension[1L], diff(g$extent[3:4])/g$dimension[2L])
  xs <- seq(g$extent[1L] + res[1L]/2, g$extent[2L] - res[1L]/2, length.out = g$dimension[1L])
  ## top to bottom
  ys <- seq(g$extent[4L] - res[2L]/2, g$extent[3L] + res[2L]/2, length.out = g$dimension[2L])
  cbind(x = xs, y = rep(ys, each = length(xs)))
}

cell_size <- function(){
  e <- gdalio_get_default_grid()$extent
  d <- gdalio_get_default_grid()$dimension
  ((e[2]-e[1])/d[1])*2
}

# ------ make a map -------------

hexmap <- function(.proj='+proj=ortho', .dim=120, .lon=0, .offset=0.7,
                   .dir=tempdir(), out_path = NULL){

  # Get fonts...
  font_add_google(name = "Bebas Neue", family = "Bebas Neue")

  gdalio_set_default_grid(list(extent=c(-1,1,-1,1)*0.7e7,
                               projection= sprintf('%s +lon_0=%s', .proj,.lon), #'+proj=rouss', #'+proj=boggs', #'+proj=laea',
                               dimension=c(x=.dim, y=.dim)))

  hex_array <- gdalio_data_hex(server_file("wms_bluemarble_s3_tms"),
                               resample='Cubic')

  hex_points <- cbind(.gdalio_grid_coords_cent(), hex_array) %>%
    as.data.frame() %>%
    mutate_at(c('x', 'y'), as.numeric) %>%
    bind_cols(sf_point(., "x", "y")) %>%
    st_as_sf()

  # generate a Hexagonal grid equal to extent of points.
  HEXgrid <- st_make_grid(hex_points,
                          cellsize = cell_size(),
                          square=F) %>%
    st_as_sf()

  # Join points and grid.
  HEXjoin <- st_join(HEXgrid, hex_points,.predicate=st_nearest,  largest=T)%>%
    filter(hex_array != '#000000')

  # Make the plot.
  showtext_auto()

  p <- ggplot(HEXjoin, aes(fill=hex_array)) +
    geom_sf(colour=alpha('grey90', 0.3), lwd=0.2) +
    scale_fill_identity() +
    guides(fill='none') +
    labs(title="The World in Hexagons...",
         caption ='#30DayMapChallenge   @hughagraham')+
    theme_void() +
    theme(title = element_text(family='Bebas Neue', face='bold', size=55, colour='grey10'),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(size=35, hjust = 0.5),
          plot.background = element_rect(fill = "grey90", colour = 'grey90'),
          panel.background = element_rect(fill = 'grey90', colour = 'grey90'))

  if (!is.null(out_path)){
    ggsave(filename = out_path, p, dpi=300)
  } else {
    f_name <- file.path(.dir, sprintf('img_%s.jpg',.lon))
    ggsave(filename = f_name, p, dpi=300)
    return(f_name)
  }

}


animate_maps <- function(long_list, .proj='+proj=ortho', .dim=120, .lon=0,
                         .offset=0.7, .dir=tempdir(), out_path, fps = 32, ncores=5){
  #set initial canvas
  gdalio_set_default_grid(list(extent=c(-1,1,-1,1)*0.7e7,
                               projection= '+proj=ortho',
                               dimension=c(x=.dim, y=.dim)))

  plan(multisession, workers = ncores)
  g <- gdalio_get_default_grid()

  l <- furrr::future_map(.x=long_list, ~hexmap(.proj=.proj, .dim =.dim, .lon=.x,
                                               .offset=.offset, .dir=.dir),

                         .progress=TRUE)

  av::av_encode_video(unlist(l), output = normalizePath(out_path),
                      framerate = fps,
                      vfilter = paste0("scale=",g$dimension[1]*20,":-2"))
  unlink(.dir)
  future:::ClusterRegistry("stop")
}


tic()
hexmap(out_path = 'exports/HexWorldStill.jpg')
toc()

# THIS IS SLOW - YOU'VE BEEN WARNED!
tic()
animate_maps(rev(seq(0, 20, by=0.5)), out_path='exports/HexWorldAnimationT.mov',
             ncores=parallel::detectCores())
toc()

