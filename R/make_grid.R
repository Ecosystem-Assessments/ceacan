#' Script to make the grid used for the analysis
#'
#' @export
make_grid <- function() {
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)

  # Canada as a line
  data(aoi)
  
  # aoi_smooth <- smoothr::smooth(smoothr::densify(aoi, max_distance = 10), method = "ksmooth")
  # canline <- sf::st_cast(aoi_smooth, "MULTILINESTRING") |>
  #            sf::st_union()    
  # 
  # canbuf <- sf::st_buffer(canline, dist = 0.5)
    
  # Cell size 
  cellsize = .5
  
  # Make grids
  grid_poly <- sf::st_make_grid(aoi, cellsize = cellsize)  
  grid_poly <- grid_poly[aoi] 
  grid_poly <- sf::st_sf(grid_poly) |>
               dplyr::mutate(uid = 1:dplyr::n())  

  # Raster grid
  grid_ras <- stars::st_rasterize(grid_poly, dx = cellsize, dy = cellsize)
  names(grid_ras) <- "uid"

  # Export
  if (!file.exists("data/data-grid/")) dir.create("data/data-grid/")
  sf::st_write(
    obj = grid_poly,
    dsn = "./data/data-grid/grid_poly.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  stars::write_stars(
    obj = grid_ras,
    dsn = "./data/data-grid/grid_raster.tif",
    delete_dsn = TRUE,
    quiet = TRUE
  )    
}
