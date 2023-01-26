#this script makes a dataframe of randomized background lon,lat points along with the corresponding covaraites
source(get_path(SCRIPT_DIR, "NarragansettLong","NarLongDF.R"))

flow_USGS <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/USGS_Water/CombinedFlow.csv") |>
  dplyr::mutate(id =  paste(lubridate::year(Date),strftime(Date, format = "%V"), sep = "_")) |>
  dplyr::group_by(id) |>
  dplyr::summarise(flow = mean(flow_mean,na.rm=TRUE))

mur2 <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/MUR/sstbackground2.csv") |>
  mutate(id = paste(year(date), strftime(date, format = "%V"), sep = "_"), year = year(date), 
         week = week(date))


murday <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/MUR/sstbackground1.csv") |>
  mutate(id = paste(year(date), strftime(date, format = "%V"), sep = "_"), year = year(date), 
         week = week(date)) |>
  rbind(mur2) 
  mur <- murday |>
  weekly(colnames(murday)[1:200])

daymetrand <-  readr::read_csv(file = get_path(DATA_DIR,"daymet","maxentmet.csv")) |>
  dplyr::filter(measurement == "prcp..mm.day.") |>
  dplyr::mutate(woy =  strftime(as.Date(year_day, format = "%Y_%j"), format = "%V"), id = paste(year, woy, sep = "_")) |>
  group_by(site, id) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::rename("prcp..mm.day." = value)


  
randpoint <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/daymet/randpoint.csv") |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
  dplyr::rename("date" = Date) |>
  mutate(id = paste(year(date), strftime(date, format = "%V"), sep = "_") )|>
  dplyr::mutate(site = seq(1:200))

otherenv <- count_env |>
  dplyr::select(MaxWSpd, Wdir, WSpd, flow_mean, TotPAR, id) |>
  distinct(id, .keep_all = TRUE)


ix <- match(randpoint$id, mur$id)

dtsst <- data.frame() 
for(i in 1:length(randpoint$id)){
  x <- mur[ix[i], i+1]
  dtsst[i,1] <- x
}

randsst <- randpoint |>
  cbind(dtsst$`1`) |>
  dplyr::mutate(sst = dtsst$`1` - 273.15) |>
  dplyr::select(-5) |>
  left_join(daymetrand, by = c("site", "id")) |>
  left_join(otherenv, by = "id") |>
  left_join(flow_USGS, by = "id")|>
  write_csv("randptsdf.csv")


 
