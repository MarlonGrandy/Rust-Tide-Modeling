#this script makes a dataframe of covariates during a time when no known blooms occurred. 

narmet <- read_metmax() |>
  dplyr::rename("Date" = DateTimeStamp) |>
  dplyr::mutate(`Date` =  as.Date(`Date`,format="%m/%d/%Y",tz=Sys.timezone())) 

narmetday <- data_byday(narmet,TRUE, totalnames = c("TotPrcp", "TotPAR"), avgnames = colnames(narmet)[8:21])
narmetday$id <- paste(lubridate::year(narmetday$Date),strftime(narmetday$Date, format = "%V"), sep = "_")
narmetweek <- weekly(narmetday, colnames(narmetday)[2:15],TRUE, c("TotPrcp", "TotPAR"))


mu <- readr::read_csv(file = get_path(DATA_DIR, "MUR", "pointmapsst2020.csv")) |>
  mutate(id = paste(year(date), strftime(date, format = "%V"), sep = "_"), year = year(date), 
         week = week(date))



daymet2020 <-  readr::read_csv(file = get_path(DATA_DIR,"daymet","daymetmap2020.csv")) |>
  dplyr::filter(measurement == "prcp..mm.day.") |>
  dplyr::mutate(woy =  strftime(as.Date(year_day, format = "%Y_%j"), format = "%V"), id = paste(year, woy, sep = "_")) |>
  group_by(site, id) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::rename("prcp..mm.day." = value)



randptmap <- pointsselectexp() |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2])

randmapdata <- readr::read_csv(file = get_path(DATA_DIR, "maxent", "pointsmapdata3.csv")) |>
  mutate(lon = randptmap$lon, lat = randptmap$lat, id = "2020_23")




ix <- match(randmapdata$id, mu$id)

dtsst <- data.frame() 
for(i in 1:length(randmapdata$id)){
  x <- mu[ix[i], i+1]
  dtsst[i,1] <- x
}

#making the final dataframe and filtering for sea surface temp less than 400. Weird SST values were produced if
#the SST data did not spatially cover a lon, lat point and I did not have enough time to alter the lon,lat values to get
#correct SST values. 

randmapcomnobloom <- randmapdata |>
  cbind(dtsst$`2`) |>
  dplyr::mutate(sst = dtsst$`2` - 273.15) |>
  dplyr::select(-12) |>
  left_join(daymet2020, by = c("site", "id")) |>
  dplyr::mutate(prcp_cum = prcp..mm.day.) |>
  dplyr::select(-prcp..mm.day.) |>
  dplyr::filter(sst < 400)




