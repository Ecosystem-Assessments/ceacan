#' Script to get the area of interest for the assessment
#'
#' @export
get_aoi <- function() {
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)

  # Load canada
  can <- pipedat:::basemap$can |>
         sf::st_make_valid()

  # Load canadian exclusive economic zone 
  uid <- "004b3c51"
  pipedat(uid)
  eez <- importdat(uid)[[1]] |>
         sf::st_make_valid()
         
  # -----
  aoi <- sf::st_union(eez,can)
  
  # -----
  area_thresh <- units::set_units(100000, km^2)
  aoi <- smoothr::fill_holes(aoi, threshold = area_thresh) |>
         sf::st_geometry() |> 
         sf::st_cast("POLYGON")       
  aoi <- aoi[1]
  aoi <- smoothr::fill_holes(aoi, threshold = area_thresh)
  
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