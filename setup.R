suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(terra)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(leaflegend)
  library(ghrsst)
  library(nnet)
  library(NeuralNetTools)
  library(plyr)
  library(dataRetrieval)
  library(stars)
  library(xyzt)
  library(dismotools)
  library(raster)
  library(ncdf4)
  library(tidyr)
  library(tidymodels)
  library(daymetr)
  library(lubridate)
  library(readr)
  library(rnoaa)
  library(ggplot2)
  library(rnoaa)
  library(readxl)
  library(data.table)
  library(tidyverse)
  library(tidymodels)
  library(themis)
  library(vip)
  library(unpivotr)
  library(rJava)
  library(dismo)
  library(raster)
  library(rgdal)
  library(tidyverse)
  library(rgeos)
  library(scales)
  library(fasterize)
  library(readxl)
  library(lubridate)
})


PROJECT_DIR <- "/mnt/ecocast/projects/students/mgrandy/Project"
DATA_DIR <-  "ProjectData"
FUN_DIR <- "r"
SCRIPT_DIR <- "scripts"

#' building file paths for the project
#' @param ... pass on to file.path
#' @param root char giving project directory 
get_path <- function(...,root = PROJECT_DIR){
  file.path(root, ...)
}

source_fun <- function(path = get_path(FUN_DIR)){
  ff <- list.files(path, full.names = TRUE)
  invisible(sapply(ff, source))
}

source_fun()


