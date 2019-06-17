#' data frame to global half-degree raster
#'
#' Convert data.frame to global 0.5 deg raster
#'
#' @param df data frame containing col_cellid OR cols_lonlat and value column
#' @param col_value column of value to convert to raster
#' @param tif optional output tif to write
#' @param col_cellid column of col_cellid in data frame, default "cellid", NULL if using cols_lonlat
#' @param cols_lonlat columns of cols_lonlat in data frame, eg `c("CenterLat","CenterLong")`
#'
#' @return raster of given value in data.frame by location given for half-degree global raster
#' @export
#'
#' @examples
df_to_raster <- function(df, col_value="value", tif=NULL, col_cellid="cellid", cols_lonlat = NULL){

  # df=cells_nspp_2100; col_value="nspp"; tif=tif; col_cellid="cellid"; cols_lonlat = NULL

  library(raster)
  library(dplyr)
  library(readr)

  stopifnot(!is.null(cols_lonlat) | !is.null(col_cellid))
  if (!is.null(col_cellid)){
    stopifnot(length(col_cellid) == 1)
    stopifnot(col_cellid %in% names(df))
  }
  if (!is.null(cols_lonlat)){
    stopifnot(length(cols_lonlat) == 2)
    stopifnot(all(col_cellid %in% names(df)))
  }

  if (dplyr::is_grouped_df(df)) df <- ungroup(df)

  if (!is.null(cols_lonlat)){
    # get cellid
    r_na <- raster_na()
    df <- df %>%
      dplyr::mutate(
        cellid = raster::cellFromXY(r_na, matrix(data = c(CenterLong, CenterLat), ncol=2)))
    col_cellid <- "cellid"
  }

  df <- df %>%
    dplyr::rename(
      cellid = !!col_cellid,
      value  = !!col_value) %>%
    dplyr::filter(
      !is.na(cellid),
      !is.na(value)) %>%
    dplyr::select(cellid, value)

  # assign to raster
  r <- raster_na()
  #browser()
  #sum(!raster::validCell(r, df$cellid))
  #df$cellid[!raster::validCell(r, df$cellid)]
  r[df$cellid] <- df$value

  # write raster
  if (!is.null(tif)) raster::writeRaster(r, tif, overwrite=T)

  r
}

#' get global half-degree raster with NAs
#'
#' @return global half-degree raster populated with NAs
#' @export
#'
#' @examples
raster_na <- function(){
  r_na <- raster(
    xmn = -180, xmx = 180, ymn = -90, ymx = 90,
    resolution=0.5, crs=leaflet:::epsg4326)
}

