#' get IUCN Red List information
#'
#' Return data.frame of extinction risk using IUCN RedList web service
#'
#' @param genus_species
#' @param rl_token RedList token, issued <http://apiv3.iucnredlist.org/api/v3/token>
#'
#' @return data frame of: category, criteria, population_trend, published_year
#' @seealso \code{\link{rredlist::rl_search}}
#' @importFrom rredlist rl_search
#' @export
#'
#' @examples
#' rl_token <- readLines("~/private/iucn_api_key")
#' rl_search("Megaptera novaeangliae", key=rl_token)
get_iucn <- function(genus_species, rl_token){

  library(rredlist)

  res <- rredlist::rl_search(genus_species, key=rl_token)

  if (is_empty(res$result)){
    y <- data.frame(
      category         = NA,
      criteria         = NA,
      population_trend = NA,
      published_year   = NA)
  } else {
    y <- dplyr::select(
      res$result,
      category, criteria, population_trend, published_year)
  }
  y

}

#' convert proper Group to grp abbreviation for file naming
#'
#' @param group proper name of taxonomic group
#'
#' @return lower case group, with " & " replaced by ".n." and spaces replaced by
#'   "."
#' @export
#'
#' @examples
group_to_grp <- function(group){
  v <- ifelse(is.na(group), "na", group)

  v <- stringr::str_to_lower(v) %>%
    stringr::str_replace_all(" & ", ".n.") %>%
    stringr::str_replace_all(" ", ".")
  v
}

#' convert grp abbreviation to proper Group for file naming
#'
#' @param grp taxonomic group abbreviation
#'
#' @return title case group, with ".n." replaced by " & ", and "." replaced by
#'   spaces
#' @export
#'
#' @examples
grp_to_group <- function(grp){
  v <- ifelse(is.na(group), "na", group)

  v <- stringr::str_to_title(grp) %>%
    stringr::str_replace_all(fixed(".n.")," & ") %>%
    stringr::str_replace_all(fixed("."), " ")
  v
}


#' Calculate number of species (nspp)
#'
#' @param con database connection
#' @param tif raster tif output
#' @param col_grps column in species table with taxonomic groupings
#' @param group taxonomic group
#' @param tbl_spp_cells species_cells table with modeling period (ie now or 2100)
#' @param col_spp column in species_cells matching spp.SPECIESID
#' @param spp_prob_threshold probability threshold (default=0.5) to count as present
#'
#' @return True of completed, False if not
#' @export
calc_nspp <- function(con, tif, col_grps, group, tbl_spp_cells, col_spp, spp_prob_threshold=0.5){

  cells_nspp <- tbl(con, "spp") %>%
    filter(!!sym(col_grps) == !!group) %>%
    select(SPECIESID) %>%
    left_join(
      tbl(con, tbl_spp_cells) %>%
        select(
          !!col_spp, cellid, probability) %>%
        filter(
          probability >= spp_prob_threshold),
      by=c("SPECIESID"=col_spp)) %>%
    group_by(cellid) %>%
    summarize(
      nspp = n()) %>%
    collect()
  r <- df_to_raster(cells_nspp, "nspp", tif)
  #plot(r, col = cols, main=glue("nspp {col_grps}-{grp}-{tbl_spp_cells}"))
  T
}

#' Calculate Red List Index (rli)
#'
#' @param con database connection
#' @param tif raster tif output
#' @param col_grps column in species table with taxonomic groupings
#' @param group taxonomic group
#' @param tbl_spp_cells species_cells table with modeling period (ie now or 2100)
#' @param col_spp column in species_cells matching spp.SPECIESID
#' @param spp_prob_threshold probability threshold (default=0.5) to count as present
#'
#' @return True of completed, False if not
#' @export
calc_rli <- function(con, tif, col_grps, group, tbl_spp_cells, col_spp, spp_prob_threshold=0.5){

  x <- tbl(con, "spp") %>%
    filter(groups04 == "Marine Mammals") %>% collect()

  spp_grp_w <- tbl(con, "spp") %>%
    filter(
      !!sym(col_grps) == !!group,
      !is.na(iucn_weight),
      iucn_weight != 0) %>%
    collect()

  if (nrow(spp_grp_w) == 0){
    message(glue("        all iucn_weight == NA"))
    return(F) }

  # calculate avg RedList sum of weights
  cells_rli <- tbl(con, "spp") %>%
    filter(
      !!sym(col_grps) == !!group,
      !is.na(iucn_weight),
      iucn_weight != 0) %>%
    left_join(
      tbl(con, tbl_spp_cells) %>%
        select(
          !!col_spp, cellid, probability) %>%
        filter(
          probability >= spp_prob_threshold),
      by=c("SPECIESID"=col_spp)) %>%
    group_by(cellid) %>%
    summarize(
      rls    = sum(iucn_weight, na.rm = T),
      nspp_w = n()) %>%
    collect() %>%
    mutate(
      rli = rls / nspp_w)

  r <- df_to_raster(cells_rli, "rli", tif)
  #plot(r, col = cols, main=glue("rli {col_grps}-{grp}-{tbl_spp_cells}"))
  T
}
