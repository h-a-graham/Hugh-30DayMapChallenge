# plot(st_geometry(wales_sf))

set_canvas_sf(wales_sf)
get_canvas(1000)
tc <- topo_matrix(100, src='aws')
ov <- map_drape(50, src='wms_arcgis_mapserver_ESRI.WorldImagery_tms',alpha=0.6)

walesMap <- tc %>%
  sphere_shade(texture='imhof4')%>%
  add_overlay(., ov,rescale_original=T) %>%
  add_shadow(texture_shade(tc, detail=0.5, contrast = 4, brightness = 8),0) %>%
  add_overlay(., generate_line_overlay(wales_sf, extent=canvasExent(),
                                       heightmap = tc, color ='#73A7E1',
                                       linewidth=0.5),
              rescale_original=F)
plot_3d(walesMap, tc, zscale=100)
render_camera(theta=0, phi=89, zoom=0.6, fov=0)

# environment_light = "quarry_03_4k.hdr"
cache_f <- tempfile()
render_highquality(filename = 'exports/Day5OSM.png',
                   lightdirection = c(60,90, 240),
                   lightaltitude=c(90,25, 12),
                   lightintensity=c(100, 500, 450),
                   lightcolor = c("white", "#FF9956", "#73A7E1"),
                   environment_light = 'data/syferfontein_1d_clear_4k.hdr',
                   title_text='Afonydd Cymru',
                   cache_filename=cache_f)

plot_map(walesMap)

save_png(walesMap, filename = 'exports/Day5OSM.png')

png(filename='exports/Day5OSM.png', width = 20, height=30, units='cm')
plot_map(walesMap)
dev.off()

