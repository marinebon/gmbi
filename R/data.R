#' Global marine biodiversity indicators from AquaMaps and IUCN
#'
#' Raster stack containing global half-degree rasters with species richness
#' (\code{nspp_\*}) and Red List Sum (\code{rls_\*}) for current distributions
#' and 2100 based on IPCC A2 emissions scenario.
#'
#' @format A \code{\link[raster]{stack}} with indicator (\code{nspp_*} or
#'   \code{rls_\*}) and taxonomic group or taxa (\code{\*_all}).
#' @source \url{https://github.com/marinebon/bbnj}
#' @source \url{https://aquamaps.org}
#' @source \url{https://iucnredlist.org}
"gmbi_stack"

#' AquaMaps species list
#'
#' Table of AquaMaps species
#'
#' @format A data.frame with species information.
#' @source \url{https://github.com/marinebon/bbnj}
#' @source \url{https://aquamaps.org}
spp <- readr::read_csv(here("inst/data/spp.csv"))
usethis::use_data(spp, overwrite = TRUE)

#' IUCN species information
#'
#' Table of IUCN species information
#'
#' @format A data.frame with species information.
#' @source \url{https://github.com/marinebon/bbnj}
#' @source \url{https://iucnredlist.org}
spp_iucn <- readr::read_csv(here("inst/data/spp_iucn.csv"))
usethis::use_data(spp_iucn, overwrite = TRUE)
