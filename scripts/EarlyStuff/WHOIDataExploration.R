#This script is an assortment of functions and graphs used as a preliminary exploration of data produced from WHOI buoys
suppressPackageStartupMessages({
  required <- c("dplyr", "ggplot2", "rnoaa", "readr", "scales", "stats","tidyverse","dtw") 
  installed <- installed.packages() |>
    dplyr::as_tibble()
  needed <- !(required %in% installed$Package)
  if(any(needed)){
    install.packages(required[needed])
  }
  ok <- sapply(required, library, character.only = TRUE)
})


##source to other files
source("/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/habhub/habhub.r")
source("/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NDBC/NDBC.r")
source("/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/URIData.r")
#source("/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NarragansettMeteorlogical/NARPCMET.r")

if(1==2){
##reads in the abundance and environmental variable data from functions and csv files
habhub <-read_csv("/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/habhub/habhub.csv.gz")
habhub_data <- read_habhub() |>
  dplyr::rename("DateTime" = `datetime`)
met_dataRI <- readr::read_csv(file="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NDBC/NDBC_QPTR1.csv.gz")

#NARPCMET <-readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NarragansettMeteorlogical/NARPCMET.csv")
##_________________________________________________________________________________________________________________

#graph all abundance data from various sources in single plot
  habhub_plot <-
    ggplot(data = filter(habhub_data, station == "URIDock"), aes(`DateTime`,`abundance`)) +
    geom_line(aes(color="URIDock")) +
    geom_line(data = filter(habhub_data, station == "URIDock"),  aes(`DateTime`,`abundance`, color = "URIDock")) +
    geom_line(data = filter(habhub_data, station == "FiddlersCove"),  aes(`DateTime`,`abundance`, color = "FiddlersCove")) +
    geom_line(data = filter(habhub_data, station == "MarthasVineyard"),  aes(`DateTime`,`abundance`, color = "MarthasVineyard")) +
    geom_line(data = filter(habhub_data, station == "Bowdoin"),  aes(`DateTime`,`abundance`, color = "Bowdoin")) + 
   geom_line(data = URI_count_data,  aes(`DATE`,`Cochlodinium`, color = "URI_longterm")) + 
    scale_x_datetime(date_labels = "%b-%d-%Y")


##_________________________________________________________________________________________________________________


#' group data by day and summarize cols.
#' @param x the desired dataset 
#' @param sum_names names of columns to summarize
#' @param date_name name of date column in timesereies
#' @return tibble of data
data_byday <- function(x, sum_names=c("abundance")){
  new_data <- x |>
    dplyr::mutate(`DateTime` = as.Date(`DateTime`)) |>
    dplyr::group_by(`DateTime`) |>
    dplyr::summarise_at(sum_names, mean, na.rm =TRUE)
  return(new_data)
}


##_________________________________________________________________________________________________________________

##autocorrelation for abundance data with original ~hourly resolution
URI_count_data["Cochlodinium"] |>
  acf(lag.max = 56, na.action = na.pass, main = "ACF Long-Term Timeseries (original)" )

URIdock |>
  acf(lag.max = 1000, na.action = na.pass, main = "ACF HabHub URIDock (original)" )

cove |>
  acf(lag.max = 1000, na.action = na.pass, main = "ACF HabHub FiddlersCove (original)" )

vineyard |>
  acf(lag.max = 1000, na.action = na.pass, main = "ACF HabHub Martha's Vineyard (original)" )

bowdoin |>
  acf(lag.max = 1000, na.action = na.pass, main = "ACF HabHub bowdoin (original)" )

##autocorrelation for abundance data with daily resolution
data_byday(URIdock) |>
  acf(lag.max = 600, na.action = na.pass, main = "ACF URI Long-Term Phyto. Timeseries (daily)" )

data_byday(cove) |>
  acf(lag.max = 600, na.action = na.pass, main = "ACF HabHub FiddlersCove (daily)" )

data_byday(vineyard) |>
  acf(lag.max = 600, na.action = na.pass, main = "ACF HabHub Martha's Vineyard (daily)" )

data_byday(bowdoin) |>
  acf(lag.max = 600, na.action = na.pass, main = "ACF HabHub bowdoin (daily)" )


  
##-----------------------------------------------------------------------------------------------------------------
  
##graph of abundance and environmental varaibles over time 
buoy_count_combine <- dplyr::left_join(daily_NDBC_RI, data_byday(habhub_data, c("abundance"))) 
compare_plot <- buoy_count_combine |>
  dplyr::filter(`abundance` < 6000) |>
  dplyr::mutate(`sea_surface_temperature` = `sea_surface_temperature` *100, `gust` = `gust` * 100, `air_pressure` = `air_pressure` ) |>
  ggplot()+ 
  geom_point( aes(x=DateTime, y = `abundance`,color="Abundance")) +
  geom_line(aes(x=`DateTime`, y = `sea_surface_temperature` ,color = "SST *100")) +
  geom_line(aes(x=`DateTime`, y =  `gust`, color = "gust *100")) 

##-----------------------------------------------------------------------------------------------------------------
}
#comparison analysis

habhub_plot2 <-
  ggplot(data = data_byday(filter(habhub_data, station == "URIDock",`abundance` < 6000)), aes(`DateTime`,`abundance`)) +
  geom_line(aes(color="URIdock")) +
  geom_line(data = data_byday(filter(habhub_data, station == "FiddlersCove",`abundance` < 6000)), aes(`DateTime`,`abundance`, color="FiddlersCove"))
show(habhub_plot2)


