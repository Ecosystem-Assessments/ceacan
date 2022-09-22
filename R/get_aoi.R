#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  # Load canada
  can <- pipedat:::basemap$can

  # Load canadian exclusive economic zone 
  uid <- "004b3c51"
  pipedat(uid)
  eez <- importdat(uid)[[1]]
  
  # ----
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)
  aoi <- dplyr::bind_rows(can,eez)
       #   sf::st_union() 
       #  https://search.r-project.org/CRAN/refmans/smoothr/html/fill_holes.html
       #   area_thresh <- units::set_units(1000, km^2)
       # p_dropped <- fill_holes(p, threshold = area_thresh)
       #        x <- sf::st_intersection(aoi)
  

  # Export data
  if (!file.exists("data/data-basemap/")) dir.create("data/data-basemap/")
  sf::st_write(
    obj = aoi,
    dsn = "./data/data-basemap/aoi.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  # Export for lazy load
  save(aoi, file = './data/aoi.RData')
}