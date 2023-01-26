source(get_path(SCRIPT_DIR, "maxent_model", "maxent.R"))
#this script runs the maxent model on a series of points taken from a time period where there were known blooms
#present in narragansett bay 

#setting the model (me) equal to a variable
model <- me
#reading in the points dataframe and filtering to only cover narragansett bay 
julydata <- randmapcom |>
  dplyr::rename("SST" = sst) |>
  filter(lon > -71.486805)

set.seed(15)
#predicting using the model on the points dataset
julypredict <- dismo::predict(model, julydata |>
                dplyr::select(Wdir, WSpd, TotPAR, SST,prcp_cum, flow,woy))|>
  as_tibble() |>
 cbind(julydata$lat, julydata$lon) |>
  dplyr::rename(lat = `julydata$lat`,lon = `julydata$lon`)

#setting na color palette
pall <- colorNumeric(
  palette = "viridis",
  domain = c(0.18, 0.32)
)
#plotting a prediction heatmap of the blooms
julymap <- julypredict |>
  leaflet() |>
  addTiles() |> 
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) |>
  addCircles(lng=~lon,lat=~lat,color = ~pall(value),radius=200) 

                              