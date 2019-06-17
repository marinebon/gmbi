
#' get database connection
#'
#' @param host defaults to "localhost"
#' @param port defaults to 5432
#' @param dbname defaults to "am"
#' @param user defaults to "admin"
#' @param password defaults to "CHANGEME!"
#'
#' @return DBI::dbConnect()
#' @export
#' @import DBI RPostgreSQL
#'
#'
#' @examples
#' con <- get_db_con(password=readLines("~/private/pg_pass"))
get_db_con <- function(
  host     = "localhost",
  port     = 5432,
  dbname   = "am",
  user     = "admin",
  password = "CHANGEME!"){

  library(DBI)
  library(RPostgreSQL)

  # psql -U admin -W -h localhost -d am
  DBI::dbConnect(
    drv      = RPostgreSQL::PostgreSQL(),
    host     = host,
    port     = port,
    dbname   = dbname,
    user     = user,
    password = password)
}

#' load csv data into database
#'
#' @param con database connection
#' @param pfx prefix for locally available data
#' @param pfx_db prefix on database server
#' @param tbl_csv named list of database table names = csv's to import
#'
#' @return TRUE
#' @export
#' @importFrom glue glue
#' @importFrom raster raster
#' @import here dplyr
#'
#' @examples
#' \dontrun{
#' csv_to_db(con)
#' }
csv_to_db <- function(
  con,
  pfx     ="/Users/bbest/docker_pg_data/aquamaps_csv",
  pfx_db  ="/data/aquamaps_csv",
  tbl_csv = list(
    spp        = "speciesoccursum_ver0816c.csv",
    obs_cells  = "occurrencecells_ver0816c.csv",
    env_cells  = "hcaf_v6.csv",
    spp_cells  = "hcaf_species_native_ver0816c.csv"),
  redo = T){

  library(dplyr)
  library(here)
  library(glue)
  library(raster)
  select = dplyr::select

  for (i in seq_along(tbl_csv)){ # i=1
    tbl    <- names(tbl_csv)[i]
    csv    <- file.path(pfx   , tbl_csv[[tbl]])
    csv_db <- file.path(pfx_db, tbl_csv[[tbl]])
    delim  <- ","

    if (tbl %in% DBI::dbListTables(con) & !redo) next
    if (tbl %in% c("env_cells")) delim = "\t"

    cat(glue("{i} {tbl}: {basename(csv)}\n\n"))

    d <- readr::read_delim(csv, delim = delim, n_max = 10, guess_max = 2000000)
    dplyr::copy_to(con, d, tbl, temporary=F, overwrite=T)

    DBI::dbExecute(con, glue::glue("DELETE FROM {tbl};"))
    DBI::dbExecute(con, glue::glue("COPY {tbl} FROM '{csv_db}' WITH DELIMITER '{delim}' CSV HEADER;"))
  }

  spp       <- dplyr::tbl(con, "spp")
  obs_cells <- dplyr::tbl(con, "obs_cells")
  spp_cells <- dplyr::tbl(con, "spp_cells")
  env_cells <- dplyr::tbl(con, "env_cells")

  # AquaMaps raster specifications for 0.5 degree global raster
  r_na <- raster::raster(
    xmn = -180, xmx = 180, ymn = -90, ymx = 90,
    resolution=0.5, crs=leaflet:::epsg4326)

  cells <- env_cells %>%
    dplyr::group_by(CenterLat, CenterLong) %>%
    dplyr::summarize() %>%
    dplyr::collect() %>%
    dplyr::mutate(
      cellid = raster::cellFromXY(
        r_na, matrix(data = c(CenterLong, CenterLat), ncol=2)))
  dplyr::copy_to(con, cells, "cells", temporary=F, overwrite=T)

  TRUE
}
