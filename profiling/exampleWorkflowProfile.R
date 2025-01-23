library(pdindicatoR)
library(sf)      # working with spatial objects
library(dplyr)
library(profvis)
library(ape)
library(rotl)

profvis({
  ex_data <- retrieve_example_data()
  tree <- ex_data$tree
  cube <- ex_data$cube
  grid <- ex_data$grid
  pa <- ex_data$pa
  
  options(width = 1000)
  plot(tree, cex = 0.35, y.lim = 100)
  
  matched <- taxonmatch(tree)

  matched_nona <- matched %>% dplyr::filter(!is.na(gbif_id))
  
  mcube <- append_ott_id(tree, cube, matched_nona)
  
  check_completeness(mcube)
  
  mcube <- mcube %>% dplyr::filter(!is.na(ott_id))
  
  PD_cube <- get_pd_cube(mcube, tree, metric = "faith")
  
  PDindicator <- generate_map_and_indicator(PD_cube, grid, "Fagales")
  
  PDindicator <- generate_map_and_indicator(
    PD_cube,
    grid,
    "Fagales",
    cutoff = 450)
  
  plots <- PDindicator[[1]]
  indicators <- PDindicator[[2]]
})

"
The only computational bottleneck that is present in the example workflow is that 
off the taxonmatch function. This function utilizes significantly more resources
and time in comparisson to the other functions present in the code, i.e. a factor 300
more computational time and up to 3 times the amount of memory.

As the data that is being processed increases this could lead to significant code slow
down and needs to remedied in order to assure code useability.
"