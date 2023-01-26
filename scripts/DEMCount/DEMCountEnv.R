#This script makes a dataframe of environmental variables that correspond to the Rhode Isalnd Deaprtment of Monitoring
# (RI DEM) rust tide sample locations
source(get_path(SCRIPT_DIR, "DEMCount", "demMET.R"))

dem <- read_csv(file = get_path(DATA_DIR, "DEMCountData", "DEMCount.csv")) |>
  mutate(id = paste(lubridate::year(Date),strftime(Date, format = "%V"), sep = "_"))

nar_met <- read_csv(file = get_path(DATA_DIR, "NarragansettMeteorlogical", "NARPCMET.csv.gz")) |>
  dplyr::rename("Date" = DateTimeStamp) |>
  dplyr::mutate(`Date` =  as.Date(`Date`,format="%m/%d/%Y",tz=Sys.timezone()))
nar_met <- data_byday(nar_met, TRUE, c("TotPrcp", "TotPAR"), colnames(nar_met)[8:21]) |>
  mutate(id = paste(lubridate::year(Date),strftime(Date, format = "%V"), sep = "_")) 
nar_met <- weekly(nar_met, colnames(nar_met)[2:15],TRUE, c("TotPrcp", "TotPAR")) 

flow. <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/USGS_Water/CombinedFlow.csv") |>
  dplyr::mutate(id =  paste(lubridate::year(Date),strftime(Date, format = "%V"), sep = "_")) |>
   dplyr::group_by(id) |>
  dplyr::summarise(flow = mean(flow_mean,na.rm=TRUE))
 
  
depth <- read_csv(file = get_path(DATA_DIR, "depth_data", "depth.csv"))


dem_env <- dem |>
  dplyr::rename(count = `Cochlodinium polykrikoides (cells/L)`) |>
  dplyr::left_join(nar_met, by = "id") |>
  dplyr::left_join(flow., by = "id") |>
  dplyr::mutate(depth = depth$Band1) |>
  dplyr::bind_cols(prcp_col$value) |>
  dplyr::rename("prcp_cum" = ...37) |>
  dplyr::select(id,Date,`Station Location`, Lat, Long,count, `Tide stage`, SST, ATemp,BP,WSpd,Wdir, TotPAR, flow, depth, prcp_cum)
  


