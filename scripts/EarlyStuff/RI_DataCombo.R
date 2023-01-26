#This is an experimental scrip to merge the WHOI Habhub buoy data with the Narragansett Bay Long Term Timeseries data.
library(dplyr)
library(tidyr)
library(readr)

#reading in files e
URI_count_data <- readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/countdata_weekly.csv.gz" )
URI_nutrient <- readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/nutrientdata_03.07.2022.csv.gz" )
URI_physical <- readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/PhysicalData_4.25.2022.csv.gz" )


#' group data by day and summarize cols.
#' @param x the desired dataset 
#' @param sum_names names of columns to summarize
#' @param date_name name of date column in timesereies
#' @return tibble of datar

data_byday <- function(x, sum_names=c("abundance")){
  new_data <- x |>
    dplyr::mutate(`DateTime` = as.Date(`DateTime`)) |>
    dplyr::group_by(`DateTime`) |>
    dplyr::summarise_at(sum_names, mean, na.rm =TRUE)
  return(new_data)
}


#filtering URI habhub data to a weekly resolution and plotting 
URI_dock_newcol <-  URIdock |> filter(`DateTime` > "2019-09-03") 
URI_dock_newcol$week <- round_date(URIdock$DateTime, "week",week_start = getOption("lubridate.week.start", 1))
URI_dock_weekly <- URI_dock_newcol |> dplyr::group_by(`week`) |>
  summarise(`Margalefidinium polykrikoides` = mean(`Margalefidinium polykrikoides`)) 
show(ggplot(data = URI_dock_weekly, aes(x=week, y=`Margalefidinium polykrikoides`, color = "Habhub URI Dock")) +
  geom_line())


#filtering Narragansett longterm timeseries to match habhub time span and plotting
URI_count_cut <- URI_count_data |>
  filter(`DATE` > "2019-09-03", `DATE` < "2021-09-14") |>
  dplyr::group_by(`DATE`) |>
  summarise(`Cochlodinium` = mean(`Cochlodinium`)) |>
  rename("week" = `DATE`)

show(ggplot(data = y, aes(x = week, y = Cochlodinium, color = "Nar. Long Term TS"))+
  geom_line())


#merging the Nar.long term ts and the ifcb habhub data
data_merged <- URI_count_cut |>
  merge(URI_dock_weekly, all = TRUE)



#correlation analysis between both timesereis 
ts_cor <- ccf(data_merged$`Cochlodinium`, data_merged$`Margalefidinium polykrikoides`, na.action = na.pass)
#show(ts_cor) 



plot_merged <- ggplot(data = data_merged, aes(x = `week`, y= `Cochlodinium` )) +
  geom_line(aes(color = "Nar. long ts")) +
  geom_line(data = data_merged, aes(x = `week`, y=`Margalefidinium polykrikoides`, color = "habhub URI dock"))
show(plot_merged)

#adding column to dataset with the longterm sereis merged with habhub data 
data_merged$abundance <-  rowMeans( data_merged |> dplyr::select(2,3), na.rm = TRUE) 

#grouping data by week to get finalized weekly sereis
data_merged <- data_merged |> dplyr::group_by(`week`) |>
  summarise(abundance = mean(`abundance`))

#adding rest of NAR. long term TS to 'data_meregd'
abundance_TS <-  URI_count_data |> 
  filter(`DATE` < "2019-09-09") |>
  select(1,2,3,4, "Cochlodinium") |>
  rename("count" = `Cochlodinium`) |>
  bind_rows(data_merged) |>
  tidyr::unite("Date", 1,6, na.rm = TRUE)
  
#averaging together both data sources counts and selecting the finalized columns
abundance_TS$abundance <- rowMeans( abundance_TS |> dplyr::select(5,6), na.rm = TRUE) 
abundance_TS_quant <- abundance_TS |>
  dplyr::select(1,2,3,6)


abund_plot <- ggplot(data = abundance_TS, aes(x = `Date`, y = `abundance`)) +
  geom_point()


#describing the dataset
dim_full <- dim(abundance_TS_quant)

dim_nozero <- dim(abundance_TS_quant |>
  filter(`abundance` > 0))





#Creating a new data.frame with  absent vs. present
#seen <- seen |> select(1,2,3,"Cochlodinium") |>
 # mutate(`Cochlodinium` = if_else(!is.na(`Cochlodinium` | `Cochlodinium` != "x"),1,0))
#abundance_TS_class<-mutate(abundance_TS, `abundance` = if_else(`count` > 0, 1, 0))

y <- URI_count_data |>
  filter(`DATE` > "2015-01-01")

x <- ggplot(data = y, aes(x = `DATE`, y = `Cochlodinium`)) +
  geom_line()







