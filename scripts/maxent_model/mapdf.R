#this script makes a dataframe of covariates during a time when known blooms occurred. 

narmetb <- read_metmax() |>
  dplyr::rename("Date" = DateTimeStamp) |>
  dplyr::mutate(`Date` =  as.Date(`Date`,format="%m/%d/%Y",tz=Sys.timezone())) 

narmetdayb <- data_byday(narmet,TRUE, totalnames = c("TotPrcp", "TotPAR"), avgnames = colnames(narmet)[8:21])
narmetdayb$id <- paste(lubridate::year(narmetday$Date),strftime(narmetday$Date, format = "%V"), sep = "_")
narmetweekb <- weekly(narmetday, colnames(narmetday)[2:15],TRUE, c("TotPrcp", "TotPAR"))


mub <- readr::read_csv(file = get_path(DATA_DIR, "MUR", "pointmapsst2020.csv")) |>
  mutate(id = paste(year(date), strftime(date, format = "%V"), sep = "_"), year = year(date), 
         week = week(date))


daymet2020b <-  readr::read_csv(file = get_path(DATA_DIR,"daymet","daymetmap2020.csv")) |>
  dplyr::filter(measurement == "prcp..mm.day.") |>
  dplyr::mutate(woy =  strftime(as.Date(year_day, format = "%Y_%j"), format = "%V"), id = paste(year, woy, sep = "_")) |>
  group_by(site, id) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::rename("prcp..mm.day." = value)


randptmapb <- pointsselectexp() |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

randmapdatab <- readr::read_csv(file = get_path(DATA_DIR, "maxent", "nobloommap.csv")) |>
  mutate(lon = randptmap$lon, lat = randptmap$lat, id = "2020_34")


ixb <- match(randmapdatab$id, mub$id)

dtsstb <- data.frame() 
for(i in 1:length(randmapdatab$id)){
  x <- mu[ixb[i], i+1]
  dtsstb[i,1] <- x
}
#making the final dataframe and filtering for sea surface temp less than 400. Weird SST values were produced if
#the SST data did not spatially cover a lon, lat point and I did not have enough time to alter the lon,lat values to get
#correct SST values. 
randmapcom <- randmapdatab |>
  cbind(dtsstb$`2`) |>
  dplyr::mutate(sst = dtsstb$`2` - 273.15) |>
  dplyr::select(-12) |>
  left_join(daymet2020b, by = c("site", "id")) |>
  dplyr::mutate(prcp_cum = prcp..mm.day.) |>
  dplyr::select(-prcp..mm.day.) |>
  dplyr::filter(sst < 400)

  
  

