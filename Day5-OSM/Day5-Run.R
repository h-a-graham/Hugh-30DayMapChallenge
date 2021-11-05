source('Day5-OSM/Day5-OSM.R')

# Get River Data
wales_gpkg <- download_osm_gb_rivers('data', overwrite=F)

wales_riv_points <- read_sf(wales_gpkg) %>%
  st_cast('LINESTRING')

# Get elevation data
set_gdalio_sf(wales_riv_points, res=50)
# gdalio_get_default_grid()

# get data with gdalio
topo_points <- gdalio_xyz(topography_source("aws"),
                          resample='Cubic') %>%
  as.data.frame() %>%
  filter(Band1 > 0) %>%
  bind_cols(sf_point(., "x", "y")) %>%
  st_as_sf(crs=gdalio_get_default_grid()$projection)


join_elevation <- st_join(wales_riv_points, topo_points,
                          join=st_nearest_feature)

plot_rivers(join_elevation, background = '#FFD8B8',
                 out_path = 'exports/Day5-OSMRivers.jpg')

tic()
animate_maps(join_elevation, rev(seq(1, max(join_elevation$Band1, na.rm=T)-100, by=5)),
             out_path='exports/Day5-OSMRivers.mov', background = '#FFD8B8',
             fps = 40)
toc()
