#devtools::install_github("marinebon/gmbi")
#library(gmbi)
devtools::load_all("")
library(raster)
library(here)
library(glue)

# TODO: + store in data-raw so could look at with software external to R, eg QGIS

# TODO: clip to high seas

# TODO: use data(package="gmbi") in future
dir_tifs <- here("data-raw/rasters")

tifs <- list.files(dir_tifs, glue(".*\\.tif$"), full.names = T)

gmbi_indicators <- raster::stack(tifs)
usethis::use_data(gmbi_indicators, overwrite = TRUE)

# old ----
# perhaps revisit code below to programmatically use_data() on separate indicators and _all/_grps
# metrics <- c("nspp","rls")
# for (m in metrics){ # m = "nspp"
#   all_tif  <- file.path(dir_tifs, glue("{m}_all.tif"))
#
#   bio_aquamaps <- raster::stack(file.path(dir_tifs, tifs))
#   usethis::use_data(bio_aquamaps, overwrite = TRUE)
#
#
#   grp_tifs <- tibble(
#     tif = list.files(dir_tifs, glue("^{m}_.*.tif$"), full.names = T)) %>%
#     filter(tif != all_tif) %>%
#     pull(tif)
#
#     str_subset(!fixed(all_tif))
#   all_tif == grp_tifs[1]
#                file.exists(all_tif)
#
# }
# rls_all_tif <- file.path(dir_tifs,  "rls_all\\.tif", full.names = T)
# nspp_all_tif <- file.path(dir_tifs,  "rls_all\\.tif", full.names = T)
# rls_grp_tifs <- list.files(dir_tifs,   "^rls_.*.tif$", full.names = T)[-1]
# nspp_grp_tifs <- list.files(dir_tifs, "^nspp_.*.tif$", full.names = T)[-1]
#
# rls_grp_tifs <- list.files(dir_tifs,   "^rls_.*.tif$", full.names = T)[-1]
# nspp_grp_tifs <- list.files(dir_tifs, "^nspp_.*.tif$", full.names = T)[-1]
# s_nspp_groups <- stack(tifs)
# names(s_nspp_groups) <- str_replace(names(s_nspp_groups), "rls_", "")
# plot(s_nspp_groups, col = cols)
#
#
# tifs <- list.files(dir_tifs, "nspp_.*\\.tif$")
# bio_aquamaps <- raster::stack(file.path(dir_tifs, tifs))
# usethis::use_data(bio_aquamaps, overwrite = TRUE)
