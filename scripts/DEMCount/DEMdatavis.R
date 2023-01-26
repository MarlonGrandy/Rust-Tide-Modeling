#This script makes a map of the Rhode Island Department of Environmental Monitoring rust tide samples 
#along with other nvironmental data.
library(ggmap)
library(RColorBrewer)

MyMap <- get_map(location = "Narragansett, RI", maptype = "roadmap", crop = FALSE, zoom = 7)
source(get_path(SCRIPT_DIR, "DEMCount", "DEMCountEnv.R"))

demcount <- ggplot(data = dem_env, aes(x = Date, y = count)) +
  geom_point()


#' makes ggplot heatmaps
#' @return  heat maps in grid form
heatmaps <- function(){
d <- dem_env |>
  dplyr::select(Lat, Long, SST, count) 

w= 0.03 * (max(d$Lat)-min(d$Lat))
h= 0.03 * (max(d$Long)-min(d$Long))
heatmapSST <- d |>
  dplyr::filter(count > 10000) |>
  ggplot() +
  geom_tile(aes(x = Lat, y = Long,fill = SST),width=w, height=h) 

heatmapCount <- d |>
  dplyr::filter(count > 10000) |>
  ggplot() +
  geom_tile(aes(x = Lat, y = Long,fill = count),width=w, height=h) +
  scale_fill_binned(type = "viridis")

x <- gridExtra::grid.arrange(heatmapSST, heatmapCount)
return(x)
}


#' calculates lagged cor.
#' @return  correlation stats
demlag <- function(){
  data_filter <- dem_env 
  
  lagcor <- function(param1, param2){
    
    x <- ccf(param1, param2, lag = 6, na.action = na.pass)
    show(x)
    return(x)
  }
  
  lagcor(data_filter$count, data_filter$flow_mean)
  
}

pal <- colorNumeric(
  palette = "viridis",
  domain = dem_env$count
)
  
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Sampled Abundace Data Locations")
)  

leafplot<- dem_env |>
  leaflet() |>
  addTiles() |> 
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) |>
  addCircles(lng=~Long,lat=~Lat,color = ~pal(count),radius=200) |>
  addAwesomeMarkers(lat = 41.57440, lng = -71.399729, icon = icons) |>
  addLegend("bottomright", pal = pal, values = ~count,
            title = "Presence-Only Data (Cells/L)",
            opacity = 1
  ) 
  
