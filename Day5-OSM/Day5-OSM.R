library(osmextract)
library(sf)
library(sfheaders)
library(dplyr)
library(plyr)
library(ggplot2)
library(gdalio)
library(tictoc)
library(topography)
library(scico)
library(showtext)
library(furrr)
# cast to multiline - add group (i.e catchment name)
# then flatten.
# st_join nearest with elevation points.
# group by catchment
# arrange by elevation
# add index
# use index to plot stuff

# Functions:

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

#download rivers
download_osm_gb_rivers <- function(.int_dir, country = 'wales', overwrite=FALSE){
  save_path <- file.path(.int_dir,'wales.gpkg')

  if (isFALSE(overwrite)){
    if (file.exists(save_path)) return(save_path)
  }

  osm_path <- oe_download(oe_match(country)$url)
  osm_gpkg <- oe_vectortranslate(osm_path, layer = "lines")

  read_big_osm <- function(os_gpkg){ #    OR waterway = 'ditch' OR waterway = 'drain'
    os_gpkg <- st_read(os_gpkg, query = "SELECT * FROM 'lines' WHERE waterway = 'river'
  OR waterway ='brook'
  OR waterway ='stream'")%>%
      select(waterway) %>%
      st_transform(., crs=27700)
  }

  osm_rivs <- read_big_osm(osm_gpkg)
  write_sf(osm_rivs, save_path, delete_dsn =TRUE)

  return(save_path)
}


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



plot_rivers <- function(df, background='white', el.cap = 0, out_path=NULL,
                        .dir=tempdir(), .width=18, .height=17, size.trans='identity'){

  # Add fonts if needed
  font_add_google(name = "Fondamento", family = "Fondamento")
  showtext_auto() # to allow text plotting

  # get max lims <-

  #filter
  .lims <-df %>%
    st_bbox()
  df <- df %>%
    filter(Band1>el.cap)

  #plot
  p <- ggplot(df, aes(colour=Band1, size=Band1)) +
    geom_sf() +
    coord_sf(xlim=c(.lims$xmin, .lims$xmax), ylim=c(.lims$ymin, .lims$ymax)) +
    scale_colour_gradientn(colours=scico(n=255, palette = 'devon', direction = 1),
                           breaks=c(round_nearest(df, min),
                                    round_nearest(df, max)),
                           limits=c(round_nearest(df, min),
                                    round_nearest(df, max)))+
    scale_size(
      range = c(0.3, 0.05),
      trans = size.trans)+
    guides(size='none', colour=guide_colourbar(barwidth = 0.2, barheight = 10))+
    labs(colour='', title='Afonydd, a Nentydd', subtitle = 'River and Stream Elevation (m.s.l)',
         caption ='#30DayMapChallenge   @hughagraham   © OpenStreetMap contributors')+
    theme_void() +
    theme(title = element_text(family='Fondamento', face='bold', size=35,
                               colour=scico(n=255, palette = 'devon', direction = 1)[1]),
          plot.title = element_text(vjust = -50, hjust = 0.02),
          plot.subtitle = element_text(vjust = -90, hjust = 0.02,size=25, colour=
                                         scico(n=255, palette = 'devon', direction = 1)[75]),
          plot.caption = element_text(size=15, hjust = 0.5),
          legend.text = element_text(family='Fondamento', size=25, colour=
                                       scico(n=255, palette = 'devon', direction = 1)[1]),
          plot.background = element_rect(fill = background, colour = background),
          panel.background = element_rect(fill = background, colour = background),
          panel.spacing = unit(c(0, 0, 0, 0), "null"))

  #save
  if (!is.null(out_path)){
    ggsave(filename = out_path, p, dpi=300, width=.width, height=.height,
           units='cm')
  } else {
    f_name <- file.path(.dir, sprintf('img_%s.jpg',el.cap))
    ggsave(filename = f_name, p, dpi=300, width=.width, height=.height,
           units='cm')
    return(f_name)
  }


}

animate_maps <- function(df, el.vals, out_path, .scale=2000, fps=64, .dir=tempdir(),
                         background='white', ncores=parallel::detectCores()-2){
  #set initial canvas
  plan(multisession, workers = ncores)
  l <- furrr::future_map(.x=el.vals, ~plot_rivers(df, el.cap=.x, .dir=.dir,
                                                  background=background),
                         .progress=TRUE)

  av::av_encode_video(unlist(c(l, rev(l))), output = normalizePath(out_path),
                      framerate = fps,
                      vfilter = paste0("scale=",.scale,":-2"))
  unlink(.dir)
  future:::ClusterRegistry("stop")
}
















