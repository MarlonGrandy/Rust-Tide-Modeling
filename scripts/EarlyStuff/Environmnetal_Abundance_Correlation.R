#This script is an assortment of initial correlation tests and plots to explore relationships between the rust tide 
#observations in the Narragansett Bay Long Term Time Series and various environmental variables. 
library(dplyr)
library(tidyr)
library(readr)
library(plyr)
library(data.table)
#reading in data
URI_count <- readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/countdata_5.12.2022.csv.gz" ) 
  URI_count$week <- floor_date(URI_count$DATE, unit = "week", week_start = getOption("lubridate.week.start", 1)) 
URI_count_data <- URI_count |>
  group_by("week") |>
  plyr::ddply( .(week), summarize_all, mean) |>
  select(-256)
URI_phys <- readr::read_csv(file ="/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/PhysicalData_4.25.2022.csv.gz" ) 
URI_phys$week <- floor_date(URI_phys$Date, unit = "week", week_start = getOption("lubridate.week.start", 1)) 
URI_physical <- URI_phys |>
  group_by("week") |>
  plyr::ddply( .(week), summarize_all, mean) |>
  select(-15)
met <- readr::read_csv(file= "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NarragansettMeteorlogical/NARPC_MET.csv.gz")|>
  as.data.table() |>
  filter(DateTimeStamp <= "2022-05-16", DateTimeStamp >= "2001-01-08")
USGS_flow <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/USGS_Water/USGS_water.csv.gz") 


#filter count dataset
URI_cut <- URI_count_data |>
  select(1,4,"Cochlodinium","week") |>
  left_join(URI_physical, by = "week")|>
  filter(`DATE` > "2001-01-04") |>
  mutate(`DATE` = as.Date(`DATE`)) |>
  as.data.table()


count_met <- URI_cut |> left_join(met, by = "week")

#merging met data and countdata based on nearest date match
#data.table::setkey(URI_cut, week )
#setkey(met, week )
#count_met <- URI_cut[ met, roll = "nearest" ]
#count_met <- count_met  |>
#  select(`DATE`, everything())
#
#count_met$days <- as.numeric(difftime(count_met$DateTimeStamp, count_met$DATE, units = "days"))

#creating new dataframes with before_bloom, during_bloom, zero_bloom

count_met$bloom_cat <- 
  as.factor(
    ifelse(count_met$`Cochlodinium` >= 1000, 'during', 
    ifelse(count_met$`week` == "2001-02-19" | count_met$`week` == "2001-04-02" |
             count_met$`week` =="2001-04-30" | count_met$`week` == "2016-08-22" | 
              count_met$`week` == "2017-08-21" | 
             count_met$`week` == "2018-08-06" |count_met$`week` == "2020-08-10" | count_met$`week` == "2021-07-26" | 
             count_met$`week` == "2022-05-02", 'before', "zero")))

count_met <- count_met |>
 left_join(USGS_flow)
#                                                              
#

#Creating seperate data.tables with seperate abundance categories
before <- count_met |>
  filter(bloom_cat == "before")
during <- count_met |>
  filter(bloom_cat == "during")
zero <- count_met |>
  filter(bloom_cat == "zero")





#surface salinity
hist_salinity <- hist(count_met$`Surface Salinity`)

plot_salinity <- ggplot(data = count_met, aes(x=count_met$`Surface Salinity`, y = bloom_cat)) +
  geom_boxplot()

#with .05 sig. level, zero and during differ but before and zero do not
#show(wilcox.test(before$`Surface Salinity`, zero$`Surface Salinity`, alternative = "two.sided"))

#airtemp
hist_Atemp <- hist(count_met$`ATemp`)

plot_Atemp <- ggplot(data = count_met, aes(x=`ATemp`, y = bloom_cat)) +
  geom_boxplot()

#before and zero different with p = .006243 and during and zero different with p = .00036
#show(wilcox.test(during$ATemp, zero$ATemp, alternative = "two.sided", paired = FALSE))


#prcp
hist_prcp <- hist(log10(count_met$TotPrcp))

plot_prcp <- ggplot(data = count_met, aes(x=count_met$TotPrcp, y = bloom_cat)) +
  geom_boxplot(outlier.shape = NA) +
   coord_cartesian(xlim = c(0, .2))

#during and zero different with p = .000001 & before and zeroi different with p = .00052
#show(wilcox.test(before$TotPrcp, zero$ATemp, alternative = "two.sided", paired = FALSE))


#totalPar
hist_par <- hist(count_met$TotPAR)

plot_par <- ggplot(data = count_met, aes(x=TotPAR, y = bloom_cat)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(xlim = c(0, 800))
#before and zero and during and zero not statistically different
#show(wilcox.test(during$TotPAR, zero$TotPAR, alternative = "two.sided", paired = FALSE))

#bottom temperature
hist_Btemp <- hist(count_met$`Bottom Temp`)

plot_Btemp <- ggplot(data = count_met, aes(x=`Bottom Temp`, y = bloom_cat)) +
  geom_boxplot()
#during and zero different with p = .0003316 and before and zero different with p = .01813
#show(wilcox.test(before$`Bottom Temp`, zero$`Bottom Temp`, alternative = "two.sided", paired = FALSE))

#barometric pressure 
hist_pressure <- hist(count_met$BP)
plot_pressure <- ggplot(data = count_met, aes(x=`BP`, y = bloom_cat)) +
  geom_boxplot()

#during and before not statistically different 
#show(wilcox.test(during$`BP`, zero$`BP`, alternative = "two.sided", paired = FALSE))

#max wind speed
hist_maxwind <- hist(count_met$MaxWSpd)
plot_maxwind <- ggplot(data = count_met, aes(x=`MaxWSpd`, y = bloom_cat)) +
  geom_boxplot()

#during and zero p = .00023 and before and zero p = .083
#show(wilcox.test(before$`MaxWSpd`, zero$`MaxWSpd`, alternative = "two.sided", paired = FALSE))


#surface temo
hist_Stemp <- hist(count_met$`Surface Temp`)
plot_Stemp <- ggplot(data = count_met, aes(x=`Surface Temp`, y = bloom_cat)) +
  geom_boxplot()
#before and zero p = .027 and during and zero p = .00073
show(wilcox.test(during$`Surface Temp`, zero$`Surface Temp`, alternative = "two.sided", paired = FALSE))


#wind 
plot_wind <- ggplot(data = count_met, aes(x=count_met$WSpd, y = bloom_cat)) +
  geom_boxplot()

#during and zero p = .0125 and before and zero p = .1652
wilcox.test(before$`WSpd`, zero$`WSpd`, alternative = "two.sided", paired = FALSE)



#river flow
plot_flow <- ggplot(data = count_met, aes(x=count_met$X_00060_00003, y = bloom_cat)) +
  geom_boxplot()

