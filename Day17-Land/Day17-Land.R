library(raytrix)
library(rayshader)
library(scico)
library(furrr)
library(tictoc)

plot_land <- function(.long=0, scico.col= 'bamako', .direction=1, .res =4e3,
                      .dir=tempdir(), out_path = NULL, .lat=0){

  set_canvas(bounds=c(-4e6,  4e6, -4e6,  4e6 ),
             projection=sprintf('+proj=tpers +h=5500000 +lat_0=%s +lon_0=%s +azi=-23.5',.lat, .long))


  topo_mat <-raytrix::topo_matrix(res=.res, src='aws')
  topo_mat[topo_mat<0] <- 0

  pal.cols <- scico(5, palette = scico.col, direction = .direction)

  ray_tex <- topo_mat %>%
    sphere_shade(texture=create_texture(pal.cols[1], pal.cols[2], pal.cols[3],
                                        pal.cols[4], pal.cols[5],)) %>%
    add_shadow(texture_shade(topo_mat, detail=0.3, contrast = 5, brightness = 12),0.2) %>%
    add_shadow(ray_shade(topo_mat,sunangle = 85, sunaltitude = 35, zscale = .res*0.75, multicore = F),0.01) %>%
    add_overlay(generate_waterline_overlay(topo_mat, min = 0.00002, max = 0.04, smooth=2, breaks=12,
                                           falloff=1.1))

  if (is.null(out_path)){
    out_path <- file.path(.dir, sprintf('img_%s.png',.long))
  }

  png(filename = out_path, width=480*3, height=480*3)
  plot_map(ray_tex, title_text = "Earth's Land", title_font = "Bebas Neue", title_size = 120,
           title_color = 'white')
  text(1700, 35, "#30DayMapChallenge @hughagraham Data: aws terrain",
       cex=1.1, col='white')
  dev.off()
  gc()
  return(out_path)
}

animate_maps <- function(out_path, long_list, .res=4e3, .dir=tempdir(),
                         fps = 32, ncores=5, scico.col='bamako',.direction=1){

  plan(multisession, workers = ncores)

  l <- furrr::future_map(.x=long_list,
                         ~plot_land(.long=.x, scico.col= scico.col,
                                    .direction=.direction, .res =.res,
                                    .dir=.dir),

                         .progress=TRUE) #

  future:::ClusterRegistry("stop")

  av::av_encode_video(unlist(l), output = normalizePath(out_path),
                      framerate = fps,
                      vfilter = paste0("scale=",480*3,":-2"))
  unlink(.dir)

}





