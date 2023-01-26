library(ggplot2)
library(caret)
library(gridExtra)

#reading in the raw CSV files for Narragansett Bay Long Term TimeSeries 

URIData_phys <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/PhysicalData_4.25.2022.csv.gz")
URIData_nutrient <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/URIData/nutrientdata_03.07.2022.csv.gz")

#reading in data for National Estuary Research Reserve Narragansett Bay Meteorological Buoy
NarragansettMeteorlogical <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/NarragansettMeteorlogical/NARPCMET.csv.gz") |>
  dplyr::rename("Date" = DateTimeStamp) |>
  dplyr::mutate(`Date` =  as.Date(`Date`,format="%m/%d/%Y",tz=Sys.timezone()))

#reading in USGS water data
USGSwater <- readr::read_csv(file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/USGS_Water/USGS_water.csv.gz") |>
  dplyr::rename("flow" = X_00060_00003)




#selecting for blooms greater than 10,000
URIData_abun <- count_env |>
  dplyr::filter(count >= 10000) |>
  dplyr::mutate(count= scale(count)) 


#plotting abundance
abun <- ggplot(data = URIData_count, aes(x = Date, y = count)) +
  geom_line()


#2018 bloom
a <- ggplot(data = URIData_physical |> filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y = `Surface Temp`, color = "SurfaceTemp")) +
  geom_line() +
  geom_point( data = URIData_abun |> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date) , y = `count`/5000, color = "count", size = 3)) 

b <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date), y = TotPAR, color = "PAR")) +
  geom_line() + 
  geom_point( data = URIData_abun|> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

c <- ggplot(data = USGSwater |> filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date), y = flow_mean, color = "flow")) +
  geom_line() + 
  geom_point( data = URIData_abun|> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

d <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date), y = TotPrcp, color = "Prcp")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

e <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date), y = MaxWSpd, color = "wind")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y =`count`/5000, color = "count", size = 3)) 

f <- ggplot(data = URIData_nutrient|>filter(Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = as.POSIXct(Date), y = DIN, color = "DIN")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2018-06-01" & Date <= "2018-09-01"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

gridExtra::grid.arrange(c,d,e)




#2016 bloom
a2 <- ggplot(data = URIData_physical |> filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y = `Surface Temp`, color = "Surface Temp")) +
  geom_line() +
  geom_point( data = URIData_abun |> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date) , y = scale(`count`)/5000, color = "count", size = 3)) 

b2 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date), y = TotPAR, color = "PAR")) +
  geom_line() + 
  geom_point( data = URIData_abun|> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

c2 <- ggplot(data = USGSwater |> filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date), y = flow_mean, color = "flow")) +
  geom_line() + 
  geom_point( data = URIData_abun|> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

d2 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date), y = TotPrcp, color = "Prcp")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 

e2 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date), y = MaxWSpd, color = "wind")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y =`count`/5000, color = "count", size = 3)) 

f2 <- ggplot(data = URIData_nutrient|>filter(Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = as.POSIXct(Date), y =DIN, color = "DIN")) +
  geom_line() +
  geom_point( data = URIData_abun|> filter( Date >= "2016-06-01" & Date <= "2016-09-30"), aes(x = Date, y = `count`/5000, color = "count", size = 3)) 


gridExtra::grid.arrange(c2,d2,e2)




#2017 bloom
a3 <- ggplot(data = URIData_physical |> filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = Date, y =`Surface Temp`, color = "Surface Temp")) +
  geom_line()+
  ylim(0,25)

b3 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = as.POSIXct(Date), y = TotPAR, color = "PAR")) +
  geom_line() +
  ylim(0,65000)

c3 <- ggplot(data = USGSwater |> filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = as.POSIXct(Date), y = flow_mean, color = "flow")) +
  geom_line()  +
  ylim(0,850)


d3 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = as.POSIXct(Date), y = TotPrcp, color = "Prcp")) +
  geom_line() +
  ylim(0,27)

e3 <- ggplot(data = NarragansettMeteorlogical_daily |> filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = as.POSIXct(Date), y = MaxWSpd, color = "wind")) +
  geom_line()  +
  ylim(0,20)

f3 <- ggplot(data = URIData_nutrient|>filter(Date >= "2017-06-01" & Date <= "2017-09-30"), aes(x = as.POSIXct(Date), y = DIN, color = "DIN")) +
  geom_line() +
  ylim(0,10) 
gridExtra::grid.arrange(c,c2,c3)







