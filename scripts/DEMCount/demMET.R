#This script gets the daymet precipitation values for the lon,lat locations in the RI DEM samples. 
met <- readr::read_csv(file = get_path(DATA_DIR,"daymet","daymet_dem.csv")) |>
    dplyr::filter(measurement == "prcp..mm.day.") |>
    dplyr::mutate(woy =  strftime(as.Date(year_day, format = "%Y_%j"), format = "%V"), year_week = paste(year, woy, sep = "_")) |>
  group_by(site, year_week) |>
  dplyr::summarise(value = sum(value)) 
  
  
prcp_col <- read_DEM() |>
  dplyr::mutate(year_week = paste(lubridate::year(Date),  strftime(Date, format = "%V"), sep = "_")) |>
  left_join(met, by = c( "year_week" = "year_week", "Station Location" = "site")) |>
  dplyr::select(value)











