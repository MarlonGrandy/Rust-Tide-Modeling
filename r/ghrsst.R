#' Get the MUR project data path
#' 
#' @param path the default path
#' @return the path
mur_path <- function(path = get_path("ProjectData", "MUR", "daily")){
  return(path)
}

#' Get the local MUR database
#' 
#' @param path the path to the database
#' @return MUR database
mur_db <- function(path = mur_path()){
  suppressPackageStartupMessages({
    require(murtools)
  })
  
  murtools::read_database(path)
}

#' Read one or more MUR files
#' 
#' @param x a database of one or more mur records 
#' @param path the mur data path
#' @return stars object
read_mur <- function(x = mur_db() |> dplyr::filter(param == "sst"), 
                     path = mur_path()){
  suppressPackageStartupMessages({
    require(murtools)
  })
  ff <- murtools::compose_mur(x, path = path)
  stars::read_stars(ff, along = list(date = x$date))
}


#' Read the mur mask
#'
#' Unless the binarize argument is set, the pixel values are returned with the
#' following coding. 1=open-sea, 2=land, 5=open-lake, 9=open-sea with ice in
#' the grid, 13=open-lake with ice in the grid
#' 
#' @param filename the name of the mask to read
#' @return stars mask
mur_mask <- function(filename = get_path("ProjectData", "MUR", "mask", "ngst.tif"),
                     binarize = FALSE){
  x <- stars::read_stars(filename) 
  names(x) <- "mask"
  if (binarize) {
   x <- x %in% c(1,9)  # open-sea and open-sea with ice
   x$mask[!x$mask] <- NA
  }
  x
}



#' Compute the (buffered) bounding box for DEM dataset
#' @param buffer distance (in degrees) to expand beyond convex hull
#' @return polygon of a bounding box
DEM_bb <- function(buffer = 0.1){
  b <- buffer[1]
  x <- DEM_read() |> 
    sf::st_geometry() |>
    sf::st_union() |>
    sf::st_convex_hull() |> 
    sf::st_bbox() |>
    as.numeric()
  xy <- cbind(x[c(1,3,3,1,1)] + c(-b,b,b,-b,-b), 
              x[c(2,2,4,4,2)] + c(-b,-b,b,b,-b) )
  xy <- sf::st_sfc(sf::st_polygon(list(xy)))
  sf::st_sf(xy, crs = 4326)
}

#' Read DEM data with sf option
#' 
#' @param as_sf logical, if TRUE convert to sf object
#' @param coords character names of [x, y] variables for coords
#' @param crs the coordinate ref system to use if as_sf is TRUE
#' @param tibble or sf
DEM_read <- function(as_sf = TRUE, coords = c("SSTLong", "SSTLat"), crs = 4326){
  x <- suppressMessages(read_DEM())
  if (as_sf) x <- sf::st_as_sf(x,coords = c("SSTLong", "SSTLat"), crs = crs[1])
x
}


#' Pull a section of MUR data into the project directory
#'
#' For each MUR record
#'   read the file
#'   crop
#'   write
#' Save the database
#' 
#' @param bb bounding box to extract
#' @param outpath the output path to store the subsets
#' @param are_you_sure logical, if TRUE run otherwise stop
#'   This is to prevent accidentally running
pull_local_mur <- function(bb = DEM_bb(),
                           outpath = get_path("ProjectData", "MUR", "daily"),
                           are_you_sure = FALSE){
  
  if (!are_you_sure) stop("don't run this unless you are sure")
  # use require within a function, it's a little like library(...)
  suppressPackageStartupMessages({
    require(murtools)
  })
  # make sure the destination exists
  if (!dir.exists(outpath)) ok <- dir.create(outpath, recursive = TRUE)
  # this is the input path
  murpath <- murtools::mur_path("nwa")
  
  DB <- murtools::read_database(murpath) |>
    dplyr::filter(param != "sst_slope") |>   # trim
    dplyr::rowwise() |>                      # by row
    dplyr::group_map(                        # iterate!
      function(sub, key, inpath = NULL, outpath = NULL, bb = NULL){
        file <- murtools::compose_mur(sub, outpath)
        path <- dirname(file)
        if (!dir.exists(path)) ok <- dir.create(path, recursive = TRUE)
        s <- stars::read_stars(murtools::compose_mur(sub, inpath)) |>  # read
          sf::st_crop(bb) |>                                           # crop
          stars::write_stars(murtools::compose_mur(sub, outpath))         # write
        sub                                                            # return the database
      }, inpath = murpath, outpath = outpath, bb = bb) |>
    dplyr::bind_rows()                                                 # list to tibble
  
    murtools::write_database(DB, outpath)                              # write the "new database"

}


#' Pull a section of the MUR world mask
#' @param bb bounding box to extract
#' @param outpath the output path to store the subsets
#' @return mask
pull_local_mask <- function(bb = DEM_bb(),
   outpath = get_path("ProjectData", "MUR", "mask")){
  
  suppressPackageStartupMessages({
    require(murtools)
  })
  
 stars::read_stars("/mnt/ecocast/coredata/mur/mask/world.tif") |>
    sf::st_crop(bb) |>
    stars::write_stars(file.path(outpath, "ngst.tif"))  
}
