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
  if (is.na(group)) return("na")

  stringr::str_to_lower(group) %>%
    stringr::str_replace_all(" & ", ".n.") %>%
    stringr::str_replace_all(" ", ".")
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
  if (grp == "na") return(NA)

  stringr::str_to_title(grp) %>%
  stringr::str_replace_all(fixed(".n.")," & ") %>%
    stringr::str_replace_all(fixed("."), " ")
}
