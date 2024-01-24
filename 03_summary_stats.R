## ----set-params----------------------------------------------------------------------------------------------------------------------
rm(list=ls()) #Ensure empty workspace if running from beginning

#################################################
#####EVERYTHING HERE SHOULD BE SET MANUALLY######
#################################################

computing <- "local" #Sets computing location and file setup; "cyverse" or "local"
nCores <- 4

#################################################



## ----setup, echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------------
# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
packageList <- c(
  "tidyverse", #Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
  "terra", #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
  "future", "future.apply", "furrr", "doFuture", "progressr", #Futureverse! https://www.futureverse.org/packages-overview.html; https://henrikbengtsson.github.io/course-stanford-futureverse-2023/
  "sf", #New vector data package
  #"mapview", #For quick interactive mapping
  "here", #Relative path best practices
  "tictoc", #time running of processes
  "glue", #easy strings
  "tigris", #state polygons
  "remotes", #to access github libraries
  "peakRAM", #monitor peak ram
  "ggpmisc"  #For adding R^2 to plots
)





install.and.load.packages(packageList, autoinstall = "y")





## Clean workspace & set up environment ----
here::here() #Check here location
if(computing == "local") {
  here::i_am("code/03_summary_stats.qmd")
}
#OR
#setwd() #Set working directory directly
options(digits = 6) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)

#Start futureverse parallel computing
future::plan("multisession", workers = nCores)



## ----load, echo = FALSE--------------------------------------------------------------------------------------------------------------


#Set directories
#Derived data
if(computing == "local") {
  devDir <- here::here('data', 'derived')
} else if(computing == "cyverse") {
  devDir <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/derived"
}
if(computing == "local") {
  rawDir <- here::here('data', 'raw')
} else if(computing == "cyverse") {
  rawDir <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/raw"
}
#Specific derived data directory
outDir <- file.path(devDir, "summary_stats")
if (!dir.exists(outDir)){
  dir.create(outDir)
}

#Nathan outputs directory (i.e. data in)
if(computing == "local") {
  natOut <- here::here('data', 'nathan_outputs')
} else if(computing == "cyverse") {
  natOut <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/nathan_outputs"
}


#Load data

#Individual disturbance stacks
fireStackF <- file.path(devDir, 'fire_dist_stack_west.tif')
insectStackF <- file.path(devDir, 'insect_dist_stack_west.tif')
droughtStackF <- file.path(devDir, 'drought_stack.tif')

#Individual disturbance 5yr moving windows
beetle5yrF <- list.files(file.path(natOut), pattern = "beetle_totals", full.names = TRUE)
disturbance5yrF <- list.files(file.path(natOut), pattern = "disturbance_totals", full.names = TRUE)
drought5yrF <- list.files(file.path(natOut), pattern = "drought_totals", full.names = TRUE)
fire5yrF <- list.files(file.path(natOut), pattern = "fire_totals", full.names = TRUE)

#Disturbance combinatiosn 5yr moving windows
fireDrought5yrF <- list.files(file.path(natOut, "fire-dought"), pattern = "fire_drought_totals", full.names = TRUE)
fireBeetle5yrF <- list.files(file.path(natOut, "fire-beetle"), pattern = "fire_beetle_totals", full.names = TRUE)
droughtBeetle5yrF <- list.files(file.path(natOut, "drought-beetle"), pattern = "bettle_drought_totals", full.names = TRUE)




#Load ecoregion & forest data
nameJoin <- readr::read_csv(file.path(devDir, "epal3l4_name_join.csv")) #EPA ecoregion distinct name table for joins (from script 00_ForestStats.rmd)
forestL4Stats <- readr::read_csv(file.path(devDir, "forest_l4_dats.csv")) #EPA ecoregion stats on amount of forest (from script 00_ForestStats.rmd)
forestL3Stats <- readr::read_csv(file.path(devDir, "forest_l3_dats.csv")) #EPA ecoregion stats on amount of forest (from script 00_ForestStats.rmd
epaL3F <- file.path(rawDir, "us_eco_l3/us_eco_l3.shp")
epaL4F <- file.path(rawDir, "us_eco_l4/us_eco_l4_no_st.shp")





## ----aoi-----------------------------------------------------------------------------------------------------------------------------


# t <- rstac::stac("https://storage.googleapis.com/earthengine-stac/catalog/")
# t <- rstac::stac("https://storage.googleapis.com/earthengine-stac/catalog/EPA/EPA_Ecoregions_2013_L3.json")
# 
# 
# tt <- t |> rstac::collections()
# 
# ttt <- t |> rstac::get_request()
# 
# x <- rstac::assets_download("https://storage.googleapis.com/earthengine-stac/catalog/EPA/EPA_Ecoregions_2013_L3.json")

# 
# pull.aoi.data <- function(useCrs) {
#   #Load EPA ecoregion data from ecoregions package
#   epaL3 <- ecoregions::ContinentalUsEcoregion3 %>% 
#     sf::st_transform(useCrs)
#   epaL4 <- ecoregions::ContinentalUsEcoregion4 %>% 
#     sf::st_transform(useCrs)
#   
#   #Pull in state data as context
#   usa <- tigris::states() |>
#     sf::st_transform(useCrs)
#   west <- usa[usa$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),] 
#   
#   #EPA level 3 AOI
#   aoiL3Interest <- epaL3 |>
#     dplyr::group_by(us_l3name) |>
#     dplyr::summarize(geometry = st_union(geometry)) |>
#     dplyr::mutate(us_l3nameclean = gsub(" ", "", us_l3name)) |>
#     sf::st_filter(west)
#   
#   #EPA level 4 AOI, only including L4 ecoregions in the L3 ecoregions of interest
#   aoiL4Interest <- epaL4 |>
#     group_by(us_l4name, us_l3name) |>
#     summarize(geometry = st_union(geometry)) |>
#     left_join(nameJoin, join_by(us_l3name, us_l4name)) |>
#     dplyr::filter(us_l3name %in% aoiL3Interest$us_l3name)
# }





## ------------------------------------------------------------------------------------------------------------------------------------



all <- list(disturbance5yrF,
            drought5yrF,
            fire5yrF,
            beetle5yrF,
            fireBeetle5yrF,
            fireDrought5yrF,
            droughtBeetle5yrF)


test.func <- function(nms) {
  x <- terra::rast(nms)
  return(names(x))
}

get.l3.freqs <- function(nms) {
  
  #Create raster
  x <- terra::rast(nms)
  useCrs <- terra::crs(x)
  
  # #Load EPA ecoregion data from ecoregions package
  # epaL3 <- ecoregions::ContinentalUsEcoregion3 %>% 
  #   sf::st_transform(useCrs)
  # # epaL4 <- ecoregions::ContinentalUsEcoregion4 %>% 
  # #   sf::st_transform(useCrs)
  epaL3 <- sf::st_read(epaL3F) |>
    sf::st_transform(useCrs)
  
  #Pull in state data as context
  usa <- tigris::states() |>
    sf::st_transform(useCrs)
  west <- usa[usa$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),] 
  
  #EPA level 3 AOI
  aoiL3Interest <- epaL3 |>
    dplyr::group_by(US_L3NAME) |>
    dplyr::summarize(geometry = st_union(geometry)) |>
    dplyr::mutate(US_L3NAMECLEAN = gsub(" ", "", US_L3NAME)) |>
    sf::st_filter(west)
  
  #Get frequencies by ecoregion
  freqs <- x |> terra::freq(bylayer = TRUE, usenames = TRUE, zones = terra::vect(aoiL3Interest), wide = TRUE)
  return(freqs)
}

#Testing weird raster loading errors when using parallel
test <- all |> furrr::future_map(.f = test.func, .options = furrr_options(seed = TRUE)) ###THIS THROWS AN ERROR IF IT'S THE FIRST TIME IT HAS  BEEN RUN ON THE DEPLOYMENT?!?!
test <- all |> purrr::map(.f = test.func) ###BUT THEN THIS WILL WORK
test <- all |> furrr::future_map(.f = test.func, .options = furrr_options(seed = TRUE)) ###AND THEN THIS WORKS?!?!


tic('Run freqs for all rasters')
freqsAll <- all |> furrr::future_map(.f = get.l3.freqs, .options = furrr_options(seed = TRUE))
toc()


#t <- list(all[[1]], all[[2]]) |> purrr::map(.f = get.l3.freqs)

#RAM TESTING
install.packages('peakRAM')
library(peakRAM)

peakRAM::peakRAM(
t <- get.l3.freqs(disturbance5yrF)
)

fireD <- get.l3.freqs(fireStackF)

#single test.func peak ram = 1.1mb
#purrr test.func peak ram = 0.4mb
#furrr test.func peak ram = 8.5mb
#single get.l3.freqs -> crashes on CV, only appears to take up to 7GB max locally. Ramps up to 53GB on CV before 'abnormally terminated due to an unexpected crash'


readr::write_csv(t, here::here(outDir, "disturbance_5_year_freqs.csv"))
readr::write_csv(fireD, here::here(outDir, "fire_freqs.csv"))




## ------------------------------------------------------------------------------------------------------------------------------------

#Get zonal stats


#testing
test <- aoiL4Interest |>
  dplyr::filter(us_l4name == "Foothill Potholes" | us_l4name == "Western Beaverhead Mountains")




tic('Single layer freq')
t3 <- fireInsects |>
  terra::freq(bylayer = TRUE, usenames = TRUE, zones = terra::vect(test), wide = TRUE)
toc()

tic('Multi-layer freq')
t4 <- fireStack |>
  terra::freq(bylayer = TRUE, usenames = TRUE, zones = terra::vect(test), wide = TRUE)
toc()

readr::write_csv(t, file.path(outDir, "fireStackFreqL3.csv"))



