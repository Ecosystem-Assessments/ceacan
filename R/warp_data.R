#' Script to warp data to study grid
#'
#' @export
warp_data <- function() {
  halpern <- dir(
    here::here("data/data-raw/halpern_cea-4f84f0e3"),
    pattern = ".tif$",
    full.names = TRUE
  )
  
  venter <- dir(
    here::here("data/data-raw/terrestrial_human_footprint_venter-103a233e"),
    pattern = ".tif$",
    full.names = TRUE
  )
  
  files <- c(halpern, venter)
  nm <- basename(files) |> tools::file_path_sans_ext()
  
  # aoi 
  aoi <- sf::st_read("data/data-basemap/aoi.geojson")
  
  # Grid 
  grd <- stars::st_rasterize(aoi, dy = .1, dx = .1) #|> # use cell-size  
  # stars::write_stars("data/data-grid/grid.tif"))
  
  # All subsequent steps are wrapped in a loop to decrease memory usage 
  # NOTE: Still running into some memory issues for some stressors 
  # To address this: 
  # usethis::edit_r_environ()
  # add when the tab opens up in R studio, add this to the 1st line: R_MAX_VSIZE=100Gb
  for(i in 1:length(files)) {
    print(paste0(i," of ", length(files)))
    
    # Load data 
    temp <- stars::read_stars(files[i], proxy = TRUE)
    
    # Warp data 
    dat <- stars::st_warp(temp, grd)
    
    # Mask data 
    dat <- dat[aoi]
    
    # Log transformation
    dat <- log(dat + 1)
    
    # Standardize 
    md <- max(dat[[1]], na.rm = TRUE)
    dat <- dat/md
    
    # Export 
    output <- here::here("data","data-stressors")
    if(!file.exists(output)) dir.create(output, recursive = TRUE)
    stars::write_stars(
      dat,
      here::here(output, glue::glue("{nm[i]}.tif")),
      delete_dsn = TRUE
    )
  }
  
    
    

  # Cumulative data 
  stressors <- dir(
    output,
    pattern = ".tif$",
    full.names = TRUE
  ) |>
  lapply(stars::read_stars, proxy = TRUE)
  
  library(stars)
  cumul <- do.call("c", stressors) |>
           stars::st_redimension() |>
           stars::st_apply(c(1,2), sum, na.rm = TRUE)
           
  output2 <- "data/data-cumulative_exposure/"
  if(!file.exists(output2)) dir.create(output2, recursive = TRUE)         
  stars::write_stars(cumul, here::here(output2, "cumulative_exposure.tif"))
  dat <- stars::read_stars(here::here(output2, "cumulative_exposure.tif"))
  dat[[1]][dat[[1]] <= 0] <- NA
  ras_f2[[1]][ras_f2[[1]] < 0] <- NA # filter

  # Load canada
  can <- pipedat:::basemap$can |>
         sf::st_make_valid()
         
  out <- "figures/cumulative/" 
  if(!file.exists(out)) dir.create(out, recursive = TRUE)         
  png("figures/cumulative/cumulative_exposure.png", res = 400, width = 200, height = 200, units = "mm", pointsize = 24)
  par(mar = c(0,0,0,0))
  image(dat, col = viridis::viridis(100))
  plot(st_geometry(aoi), add = TRUE)
  plot(st_geometry(can), add = TRUE, border = "#000000AA")
  dev.off()
}

