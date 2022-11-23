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
  
  # Select stressors to consider 
  uid <- c(
    "halpern_cea-4f84f0e3-2013-artisanal_fishing",
    "halpern_cea-4f84f0e3-2013-demersal_destructive_fishing",
    "halpern_cea-4f84f0e3-2013-demersal_nondest_high_bycatch",
    "halpern_cea-4f84f0e3-2013-demersal_nondest_low_bycatch",
    "halpern_cea-4f84f0e3-2013-inorganic",
    "halpern_cea-4f84f0e3-2013-invasives",
    "halpern_cea-4f84f0e3-2013-night_lights",
    "halpern_cea-4f84f0e3-2013-ocean_acidification",
    "halpern_cea-4f84f0e3-2013-ocean_pollution",
    "halpern_cea-4f84f0e3-2013-oil_rigs",
    "halpern_cea-4f84f0e3-2013-pelagic_high_bycatch",
    "halpern_cea-4f84f0e3-2013-pelagic_low_bycatch",
    "halpern_cea-4f84f0e3-2013-plumes_fert",
    "halpern_cea-4f84f0e3-2013-plumes_pest",
    "halpern_cea-4f84f0e3-2013-population",
    "halpern_cea-4f84f0e3-2013-shipping",
    "halpern_cea-4f84f0e3-2013-slr",
    "halpern_cea-4f84f0e3-2013-sst",
    "halpern_cea-4f84f0e3-2013-uv",
    "terrestrial_human_footprint_venter-103a233e-Built2009",
    "terrestrial_human_footprint_venter-103a233e-croplands2005",
    "terrestrial_human_footprint_venter-103a233e-Lights2009",
    "terrestrial_human_footprint_venter-103a233e-NavWater2009",
    "terrestrial_human_footprint_venter-103a233e-Pasture2009",
    "terrestrial_human_footprint_venter-103a233e-Popdensity2010",
    "terrestrial_human_footprint_venter-103a233e-Railways",
    "terrestrial_human_footprint_venter-103a233e-Roads"
  )
  
  uid <- nm %in% uid
  files <- files[uid]
  nm <- nm[uid]
  
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
           
  output2 <- "data/data-cumulative_stressors/"
  if(!file.exists(output2)) dir.create(output2, recursive = TRUE)         
  stars::write_stars(cumul, here::here(output2, "cumulative_stressors.tif"))
  dat <- stars::read_stars(here::here(output2, "cumulative_stressors.tif"))
  dat[[1]][dat[[1]] <= 0] <- NA


  # Load canada
  can <- pipedat:::basemap$can |>
         sf::st_make_valid()
         
  out <- "figures/cumulative_stressors/" 
  if(!file.exists(out)) dir.create(out, recursive = TRUE)         
  png("figures/cumulative_stressors/cumulative_stressors.png", res = 400, width = 200, height = 200, units = "mm", pointsize = 24)
  par(mar = c(0,0,0,0))
  image(dat, col = viridis::viridis(100))
  plot(st_geometry(aoi), add = TRUE)
  plot(st_geometry(can), add = TRUE, border = "#000000AA")
  dev.off()
}

