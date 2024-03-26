

#Set directories
home <- here::here()

#Raw data
rawDir <- file.path(home, 'data/raw')

#Nathan outputs directory
natOut <- file.path(home, 'data/nathan_outputs')

#Derived data (outdirectory)
devDir <- file.path(home, 'data/derived')
if (!dir.exists(devDir)){
  dir.create(devDir)
}

#Specific derived data directory for this script's outputs
outDir <- file.path(devDir, "summary_stats")
if (!dir.exists(outDir)){
  dir.create(outDir)
}



# Package management ----

source(here::here('code/functions.R'), local = FALSE)

#Check the required libraries and download if needed
packageList <- c(
  "tidyverse", #Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  "terra", #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
  "future", "future.apply", "furrr", "doFuture", "progressr", #Futureverse! https://www.futureverse.org/packages-overview.html; https://henrikbengtsson.github.io/course-stanford-futureverse-2023/
  "parallelly", #some useful functions from 'future' were moved to 'parallelly' because they were helpful outside the future framework
  "sf", #New vector data package
  #"mapview", #For quick interactive mapping
  "tictoc", #time running of processes
  "glue", #easy strings
  "tigris", #state polygons
  "ggpmisc"  #For adding R^2 to plots
)

install.and.load.packages(packageList, autoInstall = "y")



#Load data

#Data from Nathan ----

fireInsectsF <- file.path(devDir, "fire_and_insects.tif")


#Individual disturbance stacks
fireStackF <- file.path(devDir, 'fire_dist_stack_west.tif')
insectStackF <- file.path(devDir, 'insect_dist_stack_west.tif')
droughtStackF <- file.path(devDir, 'drought_dist_stack_west.tif')

#Individual disturbance sums
fireSumsF <- file.path(natOut, 'beetle_stack.tif')
beetleSumsF <- file.path(natOut, 'fire_stack.tif')
droughtSumsF <- file.path(natOut, 'drought_stack.tif')

#Individual disturbance 5yr moving windows
beetle5yrF <- list.files(file.path(natOut), pattern = "beetle_totals", full.names = TRUE)
disturbance5yrF <- list.files(file.path(natOut), pattern = "disturbance_totals", full.names = TRUE)
drought5yrF <- list.files(file.path(natOut), pattern = "drought_totals", full.names = TRUE)
fire5yrF <- list.files(file.path(natOut), pattern = "fire_totals", full.names = TRUE)

#Disturbance combination 5yr moving windows
fireDrought5yrF <- list.files(file.path(natOut, "fire-dought"), pattern = "fire_drought_totals", full.names = TRUE)
fireBeetle5yrF <- list.files(file.path(natOut, "fire-beetle"), pattern = "fire_beetle_totals", full.names = TRUE)
droughtBeetle5yrF <- list.files(file.path(natOut, "drought-beetle"), pattern = "bettle_drought_totals", full.names = TRUE)
fireBeetleDrought5yrF <- list.files(file.path(natOut, "all-unique", pattern = "all_unique", full.names = TRUE))

#Additional data
#Load ecoregion & forest data
nameJoin <- readr::read_csv(file.path(devDir, "epal3l4_name_join.csv")) #EPA ecoregion distinct name table for joins (from script 01)
forestL4Stats <- readr::read_csv(file.path(devDir, "forest_l4_dats.csv")) #EPA ecoregion stats on amount of forest (from script 01)
forestL3Stats <- readr::read_csv(file.path(devDir, "forest_l3_dats.csv")) #EPA ecoregion stats on amount of forest (from script 01)
epaL3 <- sf::st_read(file.path(rawDir, "us_eco_l3/us_eco_l3.shp"))
epaL4 <- sf::st_read(file.path(rawDir, "us_eco_l4/us_eco_l4_no_st.shp"))

#Two small l4 ecoregions to test script
test <- epaL4 |>
  dplyr::group_by(US_L4NAME, US_L3NAME) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::left_join(nameJoin, join_by(US_L3NAME, US_L4NAME)) |>
  dplyr::filter(US_L4NAME == "Foothill Potholes" | US_L4NAME == "Western Beaverhead Mountains")

test2 <- epaL4 |>
  dplyr::group_by(US_L4NAME, US_L3NAME) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::left_join(nameJoin, join_by(US_L3NAME, US_L4NAME)) |>
  dplyr::filter(US_L3NAME == "Southern Rockies")

test3 <- epaL4 |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::left_join(nameJoin, join_by(US_L3NAME, US_L4NAME)) |>
  dplyr::mutate(UNIQUE_ID = row_number())

x <- terra::rast(beetle5yrF)
freqs <- x |> terra::freq(bylayer = TRUE, usenames = TRUE, zones = terra::vect(test2), wide = TRUE)
