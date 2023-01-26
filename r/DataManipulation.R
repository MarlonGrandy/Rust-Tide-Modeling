#functions that manipulate data 

#function to average data.frame on a weekly scale
#' @param dt the desired dataset 
#' @param totalData boolean specifiying if there is data that will be summed
#' @return table of data
weekly <- function(dt, avgcols,totalData = FALSE, totalnames = NULL){
  if(totalData){
    dt$id2 <- dt$id
    total <- dt |> 
      dplyr::select(totalnames, id2) 
    dt <- dt|> 
      dplyr::select(-totalnames, -id2) 
    sum_totals <-  total |> 
      dplyr::group_by(id2) |>
      dplyr::summarise_at(totalnames, sum, na.rm = TRUE)
  }
  dt <-  dt |> 
    dplyr::group_by(`id`) |>
    dplyr::summarise_at(avgcols, mean, na.rm =TRUE) |>
    ungroup()
  
  if(totalData){
    dt <- dt |> 
      bind_cols(sum_totals) |>
      dplyr::select(-id2)
  }
  return(dt)
}



#function averages/sums data over the day accounting for both groups that need to be summed and averaged
#' @param dt the desired data set 
#' @param totalData Boolean if there is data that needs to be totaled
#' @param totanames names of the data cols that need to be summed
#' @param avgnames names of the cols that need to be averaged 
#' @return data table of daily data
data_byday <- function(dt, totalData, totalnames, avgnames){
  if(totalData){
    dt$Date2 <- dt$Date
    total <- dt |> 
      dplyr::select(totalnames, Date2) 
    dt <- dt|> 
      dplyr::select(-totalnames, -Date2) 
    sum_totals <-  total |> 
      dplyr::mutate(`Date2` = as.Date(Date2)) |>
      dplyr::group_by(`Date2`) |>
      dplyr::summarise_at(totalnames, sum, na.rm =TRUE)
  }
  dt <-  dt |> 
    dplyr::mutate(`Date` = as.Date(Date)) |>
    dplyr::group_by(`Date`) |>
    dplyr::summarise_at(avgnames, mean, na.rm =TRUE)
  dt <- dt |> 
    bind_cols(sum_totals) |>
    dplyr::select(-Date2)
  return(dt)
}
