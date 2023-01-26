#' reads DEM count data 
#' @param filename char name of file to read
#' @return tibble of count data
read_DEM <- function(filename = get_path(DATA_DIR, "DEMCountData", "DEMCountData.csv")){
  dat <- readr::read_csv(filename[1]) |>
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) |>
    dplyr::rename(SST = `Water Temp (°F)`) |>
    dplyr::mutate(SST = (SST - 32)*5/9) |>
    dplyr::mutate(woy = date_to_week(Date))
  return(dat)
}


#' function downloads USGS water flow data
#' @param siteNumber USGS site number
#' @param parameterNumber number describing what data parameter to get
#' @param startDate stat date 
#' @param stopDate end date
#' @return returns tibble of data
get_flow <- function(siteNumber,parameterNumber, startDate, stopDate){
  x <- readNWISdv(siteNumber,parameterNumber, startDate = startDate,endDate = stopDate) 
  return(x)
}


#' function downloads URI long term plankton time series count data
#' #' @param filename char name of file to read
#' @return count data
countdata <- function(filename = get_path(DATA_DIR, "URIData", "countdata_5.12.2022.xls")){
  cnames <- c("DATE", "COUNT TYPE", "LOCATION", "Total abundance", "Achnanthes spp.", 
              "Actinocyclus sp.", "Actinomonas sp.", "Actinoptychus senarius", 
              "Akashiwo sanguineum", "Alexandrium sp.", "Amphidinium cf", "Amphidinium longum", 
              "Apedinella sp.", "Asterionella sp.", "Asterionellopsis glacialis", 
              "Asterionellopsis glacialis cf", "Asterionellopsis spp.", "Baccillaria paxillifer", 
              "Baccillaria sp.", "Bacteriastrum hyalinum", "Bacteriastrum sp.", 
              "Biddulphia sp.", "Centric unknown", "Ceratulina dentata", "Cerataulina pelagica", 
              "Cerataulina sp.", "Ceratium furca", "Ceratium fusus", "Ceratium lineatum", 
              "Ceratium lineatum cf", "Ceratium penatogonum", "Ceratium sp.", 
              "Ceratium tripos", "Chaetoceros affinis", "Chaetoceros brevis", 
              "Chaetoceros ceratosporus", "Chaetoceros coronatus cf...37", 
              "Chaetoceros compressus", "Chaetoceros compressus/contortus", 
              "Chaetoceros constrictus", "Chaetoceros contortus", "Chaetoceros coronatus", 
              "Chaetoceros coronatus cf...43", "Chaetoceros costatus", "Chaetoceros curvisetus", 
              "Chaetoceros danicus", "Chaetoceros debilis", "Chaetoceros decipiens", 
              "Chaetoceros decipiens/lorenzianus", "Chaetoceros densus", "Chaetoceros diadema", 
              "Chaetoceros didymus", "Chaetoceros didymus/protuberans", "Chaetoceros distans", 
              "Chaetoceros eibenii", "Chaetoceros fallax", "Chaetoceros furcellatus", 
              "Chaetoceros ingolfianus", "Chaetoceros laciniosus", "Chaetoceros lauderi", 
              "Chaetoceros lauderi cf", "Chaetoceros lorenzianus", "Chaetoceros mitra", 
              "Chaetoceros peruvianus", "Chaetoceros protuberans", "Chaetoceros pseudobrevis", 
              "Chaetoceros pseudocurvisetus", "Chaetoceros radicans", "Chaetoceros septontrianalis", 
              "Chaetoceros similis", "Chaetoceros simplex", "Chaetoceros socialis", 
              "Chaetoceros spp.", "Chaetoceros spp. (hyalochaetae)", "Chaetoceros spp. (phaeoceros)", 
              "Chaetoceros spp (single cell)", "Chaetoceros sp. B", "Chaetoceros subtilis", 
              "Chaetoceros tenuissimus cf", "Chaetoceros teres", "Chaetoceros teres/lauderi", 
              "Choanoflagellate", "Chrysochromulina hirta", "Chrysochromulina parkeae", 
              "Chrysochromulina pringsheimii", "Chrysochromulina spp.", "Chrysophyte", 
              "Cilliate unknown", "Cochlodinium", "Corethron criophilum", "Corethron hystrix cf", 
              "Coscinodiscus asteromphalus", "Coscinodiscus centralis", "Coscinodiscus centralis/jonesianus/radiatus cf", 
              "Coscinodiscus concinnus", "Coscinodiscus granii", "Coscinodiscus radiatus", 
              "Coscinodiscus spp.", "Coscinodiscus wailesii", "Cryptomonad", 
              "Cyclotella spp", "Cylindrotheca closterium", "Cylindrotheca spp.", 
              "Dactyliosolen blavyanus", "Dactyliosolen blavyanus/Ceratulina dentata", 
              "Dactyliosolen fragilissimus", "Dactyliosolen phuketensis", "Detonula confervacea", 
              "Detonula pumila", "Diatom unknown", "Dictyocha fibula", "Dictyocha speculum", 
              "Dictyocha speculum var polyactic", "Dictyocha spp.", "Dinobryon belgica", 
              "Dinobryon spp.", "Dinoflagellates unknown", "Dinophysis acuminata", 
              "Dinophysis norvegica", "Dinophysis spp.", "Diplopsalid", "Ditylum brightwellii", 
              "Ebria spp.", "Epiphytes", "Eucampia spp.", "Eucampia zodiacus", 
              "Euglena sp.", "Eutreptiella spp", "Eutreptiella sp. cf", "Flagellate unknown", 
              "Flagellate unknown (pear shaped)", "Goniocerus septontrianalis", 
              "Gonyaulax spp.", "Grammatophora", "Guinardia cylindrus", "Guinardia delicatula", 
              "Guinardia flaccida", "Guinardia striata", "Gymnodinium catenatum", 
              "Gymnodinium sanguineum", "Gymnodinium spp.", "Gyrodinium spirale", 
              "Gyrodinium spp.", "Heliozoan", "Hemialus spp.", "Hermesinium adriaticum", 
              "Heterocapsa rotundata", "Heterocapsa sp.", "Heterocapsa triquetra", 
              "Heterosigma sp.", "Katodinium glaucum", "Katodinium rotundatum", 
              "Katodinium spp.", "Lauderia annulata", "Leptocylindrus danicus", 
              "Leptocylindrus minimus", "Leptocylindrus spp.", "Leptocylindrus spp. cf", 
              "Licmophora spp.", "Lithodesmium intricatum", "Lithodesmium spp.", 
              "Lithodesmium undulatum", "Loricate ciliate", "Mediopyxis helysia cf", 
              "Melosira nummuloides", "Melosira spp.", "microflagellate unknowns", 
              "Navicula ostrearia", "Navicula spp", "Navicula spp. cf", "Nitzschia longissima", 
              "Nitzschia spp", "Nitzschia spp. cf", "Odontella aurita", "Odontella mobiliensis", 
              "Odontella sinensis", "Odontella spp.", "Oltmannsiella sp. cf", 
              "Oltmannsiellopsis", "Oxyphysis sp.", "Paralia sulcata", "Pennate unknown", 
              "Phaeocystis globosa", "Phaeocystis spp.", "Phaeocystis sp. colony", 
              "Plagioselmis spp", "Pleuro/Gyrosigma spp.", "Pleurosigma normanii", 
              "Pleurosigma spp.", "Polychaete larvae", "Polykrikos sp.", "Proboscia alata", 
              "Prorocentrum compressum", "Prorocentrum gracile", "Prorocentrum gracile/micans", 
              "Prorocentrum micans", "Prorocentrum minimum", "Prorocentrum scutellum", 
              "Prorocentrum triestinum", "Prorocentrum spp.", "Protoperidinium claudicans", 
              "Protoperidinium conicum", "Protoperidinium depressum/divergens", 
              "Protoperidinium divergens", "Protoperidinium leonis", "Protoperidinium oblongum", 
              "Protoperidinium oceanicum", "Protoperidinium pallidum", "Protoperidinium pallidum/pellucidum c.f.", 
              "Protoperidinium pellucidum", "Protoperidinium pyriforme", "Protoperidinium spp.", 
              "Protoperidinium subinerme", "Prymnesium sp. cf", "Pseudo-nitzschia fraudulenta", 
              "Pseudonitzschia pseudodelicatissima", "Pseudonitzschia pungens", 
              "Pseudo-nitzschia spp.", "Pseudo-nitzschia turgidula cf", "Pseudopedinella spp.", 
              "Pseudosolenia calcar-avis", "Pyramimonas spp.", "Pyramimonas torta", 
              "Pyramimonas/Tetraselmis", "Raphidophyte cf", "Rhabdonema adriaticum", 
              "Rhizosolenia hebetata", "Rhizosolenia pungens", "Rhizosolenia setigera", 
              "Rhizosolenia spp.", "Rhizosolenia striata cf", "Rhizosolenia styliformis/imbricata", 
              "Scrippsiella spp.", "Silicoflagellate", "Skeletonema spp.", 
              "Stephanopyxis sp.", "Stephanopyxis turris", "Striatella unipunctata", 
              "Thalassionema nitzschioides", "Thalassionema nitzschoides cf", 
              "Thalassionema spp.", "Thalassionema spp. cf", "Thalassiosira anguste-lineata", 
              "Thalassiosira eccentrica", "Thalassiosira kushirensis", "Thalassiosira nordenskioeldii", 
              "Thalassiosira pacifica", "Thalassiosira punctigera", "Thalassiosira rotula", 
              "Thalassiosira rotula/gravida", "Thalassiosira spp.", "Thalassiosira spp. cf", 
              "Thalassiosira spp. chain forming", "Trichodesmium sp.", "dummy")
  
  count <- readxl::read_excel(filename[1], sheet = "Count data", 
                              col_names = cnames, skip = 2, na = c("", "NaN","*data missing for 2012")
  ) |>
    dplyr::select(-dummy)
    
    return(count)
}


#' function downloads URI long term plankton time series nutrient data
#' #' @param filename char name of file to read
#' @return nutrient data
nutrientdata <- function(filename = get_path(DATA_DIR, "URIData", "nutrientdata_3.07.2022.xls")){
  cnames =  c("Date", "Depth", "NH4", "DIP", "NO3+2", "NO3", 
              "NO2", "DIN")
  shallowNutrient <- readxl::read_excel(filename, 
                                        range = cellranger::cell_limits(c(8,1), c(NA,8)), 
                                        col_names = cnames, na = c("", "NaN", "nd"))
  deepNutrient <- readxl::read_excel(filename, 
                                     range = cellranger::cell_limits(c(8,10), c(NA,17)), 
                                     col_names = cnames, na = c("", "NaN", "nd"))
  
  nutrient <- dplyr::bind_rows(shallowNutrient, deepNutrient)
  return(nutrient)
}


#' function downloads URI long term plankton time series physical data
#' #' @param filename char name of file to read
#' @return physical data
physicalData <- function(filename = get_path(DATA_DIR, "URIData", "PhysicalData_4.25.2022.xls")){
  cnames <- c("ID", "Sample ID", "Sample location", "Date", "Time", "Weather desc", 
              "Surface Temp", "Bottom Temp", "Air Temp", "Hours before high tide", 
              "Surface Salinity", "Bottom Salinity", "Secchi Depth", "...14")
  
  URIphysical <- readxl::read_excel(filename, 
                                    na = c("", "NaN", "nd", "no data"), 
                                    col_names = cnames, skip = 2)|>
    dplyr::select(-...14) |>
    select(4,7:13)
  return(URIphysical)
}


#'grabs data from national data buoy center
#'@param id string identifying the desired buoy
#'@param dataset string identifying the desired dataset
#'@param years year vector specifiying what years to collect data from 
#'@return the accosiated buoy data
get_buoy_data <- function(dataset = 'stdmet',
                          id = "QPTR1",
                          years = c(2019,2020,2021,2022),
                          csv = FALSE){
  
  get_yearly <- function(year){
    std_met <- rnoaa::buoy(dataset = dataset, buoyid = id, year = year)$data
    return(std_met)
  }                     
  
  xy <- lapply(years,get_yearly) |>
    dplyr::bind_rows() |>
    dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ")) |>
    filter(`LatinName` == "Cochlodinium polykrikoides") 
  if(csv == TRUE){
    write_csv(xy, file = paste(paste("NDBC", id, sep = "_"),"csv","gz", sep = "."))
  }
  return(xy)
  
}

#' gets Narragansett bay meteorological data
#' @param filename char name of file to read
#' @return tibble of meteorological data
read_met <- function(filename = get_path(DATA_DIR, "NarragansettMeteorlogical", "NARPCMET.csv")){
  met <- read_csv(filename, skip = 2) 
  return(met)
}


#'reads in lease data and created average column for lat and lon
#'@param filename name of path containing lease data
#'@return returns the read in lease data
read_lease <- function(filename = get_path(DATA_DIR, "LeaseData", "leaseSpatialData.csv")){
  x <- readr::read_csv(filename, show_col_types = FALSE) |>
    dplyr::mutate(lon = (Long1 + Long2 + Long3 + Long4)/4, 
                  lat = (Lat1 +Lat2 +Lat3 + Lat4)/4)
  return(x)
}



#' Reads the location data from habhubSpatialData
#' @param filename name of path containing lease data
#' @return tibble of station location data 
read_location <- function(filename = get_path(DATA_DIR, "habhub", "habhubSpatialData")){
  filename <- file.path(filename)
  x <- readr::read_csv(filename, show_col_types = FALSE)
  return(x)
}

#' creates tibble of abundance and station location and binds together all station data into long format
#' @param stations character one or more stations to read
#' @param location tibble of station and location information, see \code{\link{read_location}}
#' @param filename name of path containing lease data
#' @return long formatted tibble of station abundance and location data 

read_habhub <- function(stations = c("Barharbor", "FiddlersCove", "Bowdoin", "MarthasVineyard","URIDock"), 
                        location = read_location(), 
                        filename = get_path(DATA_DIR, "habhub") ){
  read_1 <- function(station){
    ##cat(station,"\n")
    loc <- location |>
      dplyr::filter(location %in% station)
    file = file.path(filename, paste(loc$location, "csv", sep = "."))
    x <- readr::read_csv(file, show_col_types = FALSE) |>
      dplyr::rename( "abundance"= `Margalefidinium polykrikoides`, "datetime" = `DateTime`) |> 
      dplyr::mutate(station = loc$location, lon = loc$longitude, lat = loc$latitude, .before = 1) 
    return(x)
  }
  xx <- lapply(stations, 
               read_1) |>
    dplyr::bind_rows()
  write_csv(xx, file = "/mnt/ecocast/projects/students/mgrandy/Project/ProjectData/habhub/habhub.csv.gz" )
  return(xx)
}


#' function downlaods daymet data
#' @param file_location character string of file location
#' @param start start date
#' @param end end date
#' @param simplify boolean to simplify data in tidy format
#' @return returns tibble of data
daymet <- function(start = 2015,end = 2021,simplify = TRUE){
  dat <- readr::read_csv(get_path(DATA_DIR, "DEMCountData", "DEMLandCords.csv")) |>
    dplyr::rename("lat" = LandLat, "lon" = LandLong) |>
    filter(lat != "41.228807", lat != "41.536635", lat != "41.655136")
  x <- function(x,y, site,.start = start ){
  y <- daymetr::download_daymet(lat = x,lon = y,start = .start, simplify = TRUE ) |>
    mutate(site = site)
  return(y)
  }
 
  xx <- setDF(rbindlist(mapply(x,dat$lat,dat$lon, dat$`Station Location`))) 
  return(xx)
}

pointsselect <- function(mask = 
                           mur_mask(binarize = TRUE),
                         n = 200, pts = DEM_read()){
  require(dismo)
  set.seed(15)
  pts <- pts |>
    dplyr::rename("abundance" = `Cochlodinium polykrikoides (cells/L)`) |>
    dplyr::filter( abundance > 0) |>
    dplyr::mutate(year = as.numeric(format(Date,"%Y")), 
                  doy = as.numeric(format(Date,"%j"))) 
  years <- seq(min(pts$year), max(pts$year))
  doys <- seq(min(pts$doy),max(pts$doy))
  dates <- as.Date(paste(sample(years,n, replace = TRUE), 
                         sample(doys,n, replace = TRUE)), format = "%Y %j")
  chull <- pts |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_transform("+proj=utm +zone=19 ellps=WGS84") |>
    sf::st_buffer(dist = 5000 ) |>
    sf::st_transform(4326)
  
  xypoints <- mask |> 
    as( "Raster") |>
    mask(chull |> 
           as_Spatial()) |>
    dismo::randomPoints(n = n) |>
    dplyr::as_tibble() |>
    sf::st_as_sf(crs = sf::st_crs(mask), coords = c("x", "y")) |>
    mutate(Date = dates)
  return(xypoints)
}



pointsselectexp <- function(mask = 
                           mur_mask(binarize = TRUE),
                         n = 500, pts = DEM_read()){
  require(dismo)
  set.seed(15)
  pts <- pts |>
    dplyr::rename("abundance" = `Cochlodinium polykrikoides (cells/L)`) |>
    dplyr::filter( abundance > 0) |>
    dplyr::mutate(year = as.numeric(format(Date,"%Y")), 
                  doy = as.numeric(format(Date,"%j"))) 
  
  
  chull <- pts |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_transform("+proj=utm +zone=19 ellps=WGS84") |>
    sf::st_buffer(dist = 5000 ) |>
    sf::st_transform(4326)
  
  xypoints <- mask |> 
    as( "Raster") |>
    mask(chull |> 
           as_Spatial()) |>
    dismo::randomPoints(n = n) |>
    dplyr::as_tibble() |>
    sf::st_as_sf(crs = sf::st_crs(mask), coords = c("x", "y")) 
  return(xypoints)
}



read_metmax <- function(){
  met <- read_csv(get_path(DATA_DIR,"NarragansettMeteorlogical", "NARPCMETmax.csv"), skip = 2) 
  return(met)
}





