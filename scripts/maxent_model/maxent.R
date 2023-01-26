#reading in RI DEM observatiojnal dataset with predictors
source(get_path(SCRIPT_DIR, "DEMCount", "DEMCountEnv.R"))
#reading in the background point data with bloom
source(get_path(SCRIPT_DIR, "maxent_model", "mapdf.R"))
#reading in the background point dataset with bloom
source(get_path(SCRIPT_DIR, "maxent_model", "randpointdf.R"))

#reading in the dataset used for maxent (prescense-only)
dat <- dem_env

#reaing in the background point dataset and selecting the varaibles that match the variables in "dat"
backgroundpt <- read_csv(get_path(DATA_DIR, "maxent", "randptsdf.csv")) |>
  dplyr::rename("prcp_cum" = prcp..mm.day., "SST" = sst) |>
  dplyr::mutate(woy = week(date), geometry = pointsselect()$geometry) |>
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2]) |>
  dplyr::select(Wdir, WSpd, TotPAR, SST,prcp_cum, flow,woy,lat,lon) 
  

#creating a prescence vector (1 is a prescence and 0 is a background point)
presVect <- c( rep(1,44), rep(0,200))

#creating the point data data frame with counts greater than zero and sst greater than 15. 
#After analysis, MUR assigned very low SST valoues at locations in coves that were clearly outliers, these values 
#were removed by filter due to lack of time to go get other data
pDat <- dem_env |>
  dplyr::rename("lon" = Long, "lat" = Lat) |>
  dplyr::filter(count > 0) |>
  mutate(woy = week(Date)) |>
  dplyr::select(Wdir, WSpd, TotPAR, SST,prcp_cum, flow,woy, lat, lon)|>
  rbind(backgroundpt) |>
  cbind(presVect) |>
  filter(SST > 15)

#creating training, validation, and testing folds
set.seed(15)
fold <- kfold(pDat, k=5)
occtest <- pDat[fold == 1, ]
occtrain <- pDat[fold != 1, ]

#creating the training dataset with no na
traindat <- occtrain |> 
  dplyr::select(-presVect,-lat,-lon)
trainvec <- occtrain$presVect
testvecna <- occtest |> 
  drop_na()
testvec <- testvecna$presVect
#creating the testing dataset with no na
testdatna <- occtest |> 
  drop_na()
testdat <- testdatna |>
  dplyr::select(lat,lon)

#running maxent on the training data
me <- dismo::maxent(traindat,
                    trainvec,
                    removeDuplicates = TRUE, 
                    path = get_path(DATA_DIR, "maxent", "v0.000"))


#reading the maxent output from the folder
me2 <- dismotools::read_maxent(get_path(DATA_DIR, "maxent", "v0.000"))

#evaluating the maxent validation set with evaluation metrics 
etrain <- evaluate(p=occtrain |> 
                     dplyr::filter(presVect > 0) |>
                     dplyr::select(-presVect),
                   a=occtrain |> 
                     dplyr::filter(presVect == 0) |>
                     dplyr::select(-presVect),
                   model = me)

set.seed(15)
#predicting using the trained model on the testing set and binding all data used in the 
#prediction to the prediction output
predicted <- dismo::predict(me, occtest |>
                              dplyr::select(-presVect)) |>
  as_tibble() |>
  cbind(testvec) |>
  cbind(testdat)

#evaluation metrics evaluating the testing set
etest <- evaluate(p=occtest |> 
                    dplyr::filter(presVect > 0) |>
                    dplyr::select(-presVect),
                  a=occtest |> 
                    dplyr::filter(presVect == 0) |>
                    dplyr::select(-presVect),
                  model = me)


maxplot<- predicted |>
  leaflet() |>
  addTiles() |> 
  addProviderTiles(providers$CartoDB.Voyager) |>
  addHeatmap(lng=~lon,lat=~lat,intensity=~value,max=1,radius=10,blur=10)
  

dismo::response(me)


