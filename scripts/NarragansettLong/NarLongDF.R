#This script takes the csv files read in from various data sources and creates a workflow to 
#harmonize the data in to a single data frame

#reading in the raw CSV files for Narragansett Bay Long Term Time Series  (count, physical, nutrient)

URIData_count <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/countdata_5.12.2022.csv.gz") |>
  dplyr::rename("Date" = DATE)|>
  dplyr::select("Date","Cochlodinium")|>
  mutate(Date = as.Date(Date, format = "%Y/%m/%d", tz=Sys.timezone()))|>
  dplyr::group_by(Date) |>
  dplyr::summarise(count = mean(Cochlodinium)) |>
  filter(Date >= "2001-01-01") 
  


  
  
URIData_physical <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/PhysicalData_4.25.2022.csv.gz")
URIData_nutrient <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/nutrientdata_03.07.2022.csv.gz")

#reading in data for National Estuary Research Reserve Narragansett Bay Meteorological Buoy
NarragansettMeteorlogical <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NarragansettMeteorlogical/NARPCMET.csv.gz") |>
  dplyr::rename("Date" = DateTimeStamp) |>
  dplyr::mutate(`Date` =  as.Date(`Date`,format="%m/%d/%Y",tz=Sys.timezone()))

#reading in USGS water data
USGSwater <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/USGS_Water/CombinedFlow.csv") 




#turning 15 min Narragansett Meteorological data into daily data
NarragansettMeteorlogical_daily <- data_byday(NarragansettMeteorlogical, TRUE, c("TotPrcp", "TotPAR"), colnames(NarragansettMeteorlogical)[8:21])
  

#adding identification column to data tables with year and week number 
NarragansettMeteorlogical_daily$id <- paste(lubridate::year(NarragansettMeteorlogical_daily$Date),strftime(NarragansettMeteorlogical_daily$Date, format = "%V"), sep = "_")
URIData_physical$id <-  paste(lubridate::year(URIData_physical$Date),lubridate::week(URIData_physical$Date), sep = "_")
URIData_nutrient$id <-  paste(lubridate::year(URIData_nutrient$Date),lubridate::week(URIData_nutrient$Date), sep = "_")
USGSwater$id <-  paste(lubridate::year(USGSwater$Date),strftime(USGSwater$Date, format = "%V"), sep = "_")



  
#averaging  data over weekly scale 
USGSwater_weekly<- weekly(USGSwater, "flow_mean")
NarragansettMeteorlogical_weekly <- weekly(NarragansettMeteorlogical_daily, colnames(NarragansettMeteorlogical_daily)[2:15],TRUE, c("TotPrcp", "TotPAR"))


#combining data tables into one uniform weekly table
count_env <- URIData_count|>
  mutate(id = paste(lubridate::year(Date),strftime(URIData_count$Date, format = "%V"), sep="_")) |>
  dplyr::left_join(URIData_physical, by = "id") |>
  dplyr::distinct(Date.x, .keep_all = TRUE) |>
  dplyr::left_join(NarragansettMeteorlogical_weekly, by = "id") |>
  dplyr::left_join(USGSwater_weekly, by = "id") |>
  dplyr::rename("Date" = `Date.x`) |> 
  dplyr::select(Date, id, count,`Surface Temp`, `Surface Salinity`, `Bottom Salinity`, `BP`, `WSpd`, `MaxWSpd`, `Wdir`, `flow_mean`, `TotPAR`, `TotPrcp` )










