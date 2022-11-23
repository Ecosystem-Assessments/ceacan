library(devtools)
load_all()

pipeline <- function() {
  # Update global parameters
  global_param()
  
  # Get area of interest 
  get_aoi()
  
  # Make grid 
  make_grid()
  
  # Integrate data 
  pipedat::pipeflow("./data/data-config/pipedat.yml")
  
  # Get bibliographies
  getBib()
  
  # Warp data and export cumulative stressors
  warp_data()

  # Report
  suppressWarnings(bookdown::render_book("index.Rmd", "bookdown::gitbook"))
}