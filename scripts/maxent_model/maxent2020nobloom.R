#this script runs the maxent model on a series of points taken from a time period where there were no known blooms
#present in narragansett bay to be used as a baseline map for comparison

source(get_path(SCRIPT_DIR, "maxent_model", "maxent.R"))

#setting the model (me) equal to a variable
model <- me
nobloom <- randmapcomnobloom |>
  dplyr::rename("SST" = sst) |>
  filter(lon > -71.486805)

#predicting using the model on the points dataset
set.seed(15)
nobloompred <- dismo::predict(model, nobloom |>
                                dplyr::select(Wdir, WSpd, TotPAR, SST,prcp_cum, flow,woy))|>
  as_tibble() |>
  cbind(nobloom$lat, nobloom$lon) |>
  dplyr::rename(lat = `nobloom$lat`,lon = `nobloom$lon`)


#setting na color palette
palln <- colorNumeric(
  palette = "viridis",
  domain = c(0.18, 0.32)
)

#plotting a prediction heatmap of the blooms
julymapnobloom <- nobloompred |>
  leaflet() |>
  addTiles() |> 
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) |>
  addCircles(lng=~lon,lat=~lat,color = ~palln(value),radius=200) |>
  addLegend("bottomright", pal = palln, values = ~value,
            title = "Maxent Output Value",
            opacity = 1
  ) 

