library(leaflet)
library(leaflet.extras)

 jit <-  jitter(dem_env$Lat, factor = .001)
 new_col <- jit
 dat <- dem_env |>
   cbind(new_col) |>
   dplyr::rename("LatJit" = new_col)

 pal <- colorNumeric(
   palette = "viridis",
   domain = dat$count)
 
 plot1 <- dat |>
   leaflet() |>
   addTiles() |>
   addProviderTiles(providers$CartoDB.Voyager) |>
  addCircles(lat = dat$LatJit, lng = dat$Long, color = pal, radius = 500)

 
 
#' makes ggplot heatmaps
#' @return  heat maps in grid form
plot2 <- function(){
  d <- dat |>
    dplyr::select(LatJit, Long,  count) 
  
  w= 0.03 * (max(d$LatJit)-min(d$LatJit))
  h= 0.03 * (max(d$Long)-min(d$Long))
  
  heatmapCount <- d |>
    dplyr::filter(count > 0) |>
    dplyr::rename("abundance" = count) |>
    ggplot() +
    geom_tile(aes(x = LatJit, y = Long,fill = abundance),width=w, height=h, alpha = .5) +
    scale_fill_binned(type = "viridis") +
    xlab("Latitude") +
    ylab("Longitude") +
    ggtitle("Margalefidinium Abundance Heatmap (Cells/L)") +
    theme(plot.title = element_text(size = 20)) +
   theme( plot.title = element_text(hjust = .5)) +
   theme(axis.title = element_text(size = 15))
  
 
  return(heatmapCount)
}


