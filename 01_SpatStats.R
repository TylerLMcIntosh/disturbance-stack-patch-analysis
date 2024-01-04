## ----------------------------------------------------------------------------------------------------------------------
rm(list=ls()) #Ensure empty workspace if running from beginning

#################################################
#####EVERYTHING HERE SHOULD BE SET MANUALLY######
#################################################

computing <- "local" #Sets computing location and file setup; "cyverse" or "local"
parallelFuture <- TRUE #Sets whether or not parallel computing is used; TRUE or FALSE 
aoiL3Names <- c("Southern Rockies") #The names of the EPA Level 3 ecoregions of interest, may be any length, e.g. c("Sierra Nevada", "Middle Rockies", "Idaho Batholith","Southern Rockies"). May also be NULL, in which case the AOI will be determined by calculations on data from the 00_ForestStats.Rmd script
forestPercCutoff <- 2 #The percentage of forest below which EPA regions will not be retained for calcuations, if aoiL3Names == NULL
edgeDepth <- 200 #in meters
resolution <- 30 #in meters
nCores <- 2 #number of cores if on CyVerse - future package struggles to read automatically on CyVerse, reads 128 (the maximum number of cores)

#################################################


# NO LONGER NECESSARY!

# NO LONGER NECESSARY - rstudio <- TRUE #Sets whether or not the script is being used in RStudio or not; important for CyVerse processing; TRUE or FALSE

# # #NOTE: THIS LINE MUST BE RUN BEFORE LOADING ANY LIBRARIES TO MAKE GEOSPATIAL PROJECTIONS WORK PROPERLY IN R STUDIO ON CYVERSE!
# if(computing == "cyverse" & rstudio) {
#   Sys.setenv(PROJ_LIB="/opt/conda/envs/macrosystems/share/proj")
# }



## ---- echo = FALSE, warning = FALSE, message = FALSE-------------------------------------------------------------------
# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("tidyverse", #Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
                      "terra", #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
                      "future", "future.apply", "furrr", "doFuture", "progressr", #Futureverse! https://www.futureverse.org/packages-overview.html; https://henrikbengtsson.github.io/course-stanford-futureverse-2023/
                      "sf", #New vector data package
                      #"mapview", #For quick interactive mapping
                      "here", #Relative path best practices
                      "landscapemetrics",  #Fragstats for R
                      "tictoc", #time running of processes
                      "tmap",
                      "profvis",
                      "purrr",
                      "glue", #easy strings
                      "remotes", #to access github libraries
                      "ggpmisc") #For adding R^2 to plots

#Install all packages that aren't installed yet
install.packages(setdiff(list.of.packages, rownames(installed.packages())))

#Load all packages
invisible(lapply(list.of.packages, library, character.only = TRUE)) #use 'invisible' to prevent output being displayed
#If this line throws an error on cyverse, close R session and re-open

#Install ecoregions package from github if needed, then load package
if(!"ecoregions" %in% rownames(installed.packages())) {
  remotes::install_github("tomroh/ecoregions")
}
library(ecoregions)


## Clean workspace & set up environment ----
here::here() #Check here location
if(computing == "local") {
  here::i_am("code/01_SpatStats.Rmd")
}
#OR
#setwd() #Set working directory directly
options(digits = 6) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)


## ----------------------------------------------------------------------------------------------------------------------
#Set cores
if(computing == "local") {
  nCores <- future::availableCores()
} else if(computing == "cyverse") {
  print("Using user-defined number of computing cores")
}

#Start futureverse parallel computing
if(parallelFuture) {
  future::plan("multisession", workers = nCores)
}


# A function to test a function that has been set up to run in either sequence or parallel using the futureverse, depending on what plan() has been set.
#For use in troubleshooting. This function will reset plan() to the state before the function was run
#fun = name of function as a string, e.g. "calculate.class.level.metrics"
#... = all parameters for running fun
test.future.function <- function(fun, ...) {
  
  #Get current plan
  initialPlan <- future::plan()
  
  #Test sequential version
  print("SEQUENCE")
  future::plan("sequential")
  tic(glue::glue('Time to run function {fun} in sequence'))
  sTest <- do.call(fun, list(...))
  toc()
  
  #Test parallel version
  print("PARALLEL")
  future::plan("multisession", workers = nCores)
  tic(glue::glue('Time to run function {fun} in parallel'))
  pTest <- do.call(fun, list(...))
  toc()
  
  #Check if outputs are identical
  glue("Outputs identical? - {identical(sTest, pTest)}")
  
  #Reset plan to original
  future::plan(initialPlan)
  print(glue::glue("Plan reset"))
  
}




## ---- echo = FALSE-----------------------------------------------------------------------------------------------------


#Set directories

if(computing == "local") {
  devDir <- here::here('data', 'derived')
} else if(computing == "cyverse") {
  devDir <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/derived"
}

if(computing == "local") {
  outDirT <- here::here(devDir, "metric_tables")
} else if(computing == "cyverse") {
  outDirT <- file.path(devDir, "metric_tables")
}
if (!dir.exists(outDirT)){
  dir.create(outDirT)
}

#Load data
fireInsects <- terra::rast(file.path(devDir, "fire_and_insects.tif"))
fireStack <- terra::rast(file.path(devDir, 'fire_dist_stack_west.tif'))
insectStack <- terra::rast(file.path(devDir, 'insect_dist_stack_west.tif'))
nameJoin <- readr::read_csv(file.path(devDir, "epal3l4_name_join.csv")) #EPA ecoregion distinct name table for joins (from script 00_ForestStats.rmd)
forestL4Stats <- readr::read_csv(file.path(devDir, "forest_l4_dats.csv")) #EPA ecoregion stats on amount of forest (from script 00_ForestStats.rmd)
forestL3Stats <- readr::read_csv(file.path(devDir, "forest_l3_dats.csv")) #EPA ecoregion stats on amount of forest (from script 00_ForestStats.rmd

#Load EPA ecoregion data from ecoregions package
epaL3 <- ecoregions::ContinentalUsEcoregion3 %>% 
  sf::st_transform(terra::crs(fireInsects))
epaL4 <- ecoregions::ContinentalUsEcoregion4 %>% 
  sf::st_transform(terra::crs(fireInsects))



## ----------------------------------------------------------------------------------------------------------------------
# 
# if(!is.null(aoiL3Names)) {
#   #EPA level 3 AOI
#   aoiL3Interest <- epaL3 |>
#     dplyr::filter(us_l3name %in% aoiL3Names) |>
#     dplyr::group_by(us_l3name) |>
#     dplyr::summarize(geometry = st_union(geometry)) |>
#     dplyr::mutate(us_l3nameclean = gsub(" ", "", us_l3name))
#   
#   #EPA level 4 AOI
#   aoiL4Interest <- epaL4 |>
#     dplyr::filter(us_l3name %in% aoiL3Names) |>
#     dplyr::group_by(us_l4name, us_l3name) |>
#     dplyr::summarize(geometry = st_union(geometry)) |>
#     dplyr::left_join(nameJoin, join_by(us_l3name, us_l4name))
# } else { # ADD IN AUTO - FOREST FINDER
# 
#   l3AboveForestPerc <- forestL3Stats |>
#     dplyr::filter(percForest >= forestPercCutoff) |>
#     dplyr::pull(us_l3nameclean)
#   l4AboveForestPerc <- forestL4Stats |>
#     dplyr::filter(percForest >= forestPercCutoff) |>
#     dplyr::pull(us_l4l3name)
# 
#   #EPA level 3 AOI
#   aoiL3Interest <- epaL3 |>
#     dplyr::group_by(us_l3name) |>
#     dplyr::summarize(geometry = st_union(geometry)) |>
#     dplyr::mutate(us_l3nameclean = gsub(" ", "", us_l3name)) |>
#     dplyr::filter(us_l3nameclean %in% l3AboveForestPerc)
#   
#   #EPA level 4 AOI
#   aoiL4Interest <- epaL4 |>
#     dplyr::group_by(us_l4name, us_l3name) |>
#     dplyr::summarize(geometry = st_union(geometry)) |>
#     dplyr::left_join(nameJoin, join_by(us_l3name, us_l4name)) |>
#     dplyr::filter(us_l4l3name %in% l4AboveForestPerc)
# }

#Two small l4 ecoregions to test script
test <- epaL4 |>
  dplyr::group_by(us_l4name, us_l3name) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::left_join(nameJoin, join_by(us_l3name, us_l4name)) |>
  dplyr::filter(us_l4name == "Foothill Potholes" | us_l4name == "Western Beaverhead Mountains")





## ----------------------------------------------------------------------------------------------------------------------

#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped and masked to vector
careful.clip <- function(raster, vector) {
  pack <- FALSE
  
  #Unpack if parallelized
  if(class(raster)[1] == "PackedSpatRaster") {
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if(class(vector)[1] == "PackedSpatVector") {
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  #Perform operation
  if (sf::st_crs(vector) != terra::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
    print("Projecting vector")
    vector <- sf::st_transform(vector, terra::crs(raster)) 
  } else {
    print("Vector already in raster CRS")
  }
  print("Clipping")
  r <- terra::crop(raster,
                   vector,
                   mask = TRUE) #crop & mask
  
  #Repack if was packed coming in (i.e. parallelized)
  if(pack) {
    r <- terra::wrap(r)
  }
  return(r)
}


#Function to clip a raster to a set of polygons (one clip per polygon in set),
#ensuring they are in the same projection
#Namefield indicates the field to use to name the item in the returned list (e.g. "us_l4l3name")
#Returns the set of rasters as a named list 
careful.clip.set <- function(raster, vectors, namefield) {
  splitVec <- split(vectors, f=vectors[[namefield]])
  #future::plan("sequential")
  if (is(future::plan() ,"sequential")) {
    print("Performing clip set in sequence")
    out <- splitVec |> purrr::map(careful.clip, raster = raster)
  } else { #Pack & parallelize
    print("Performing clip set in parallel")
    r <- terra::wrap(raster)
    v <- splitVec |> 
      purrr::map(terra::vect) |> 
      purrr::map(terra::wrap)
    out <- v |> furrr::future_map(careful.clip, raster = r)
    out <- out |>
      purrr::map(terra::unwrap)
  }
  #future::plan("multisession", workers = nCores)
  return(out)
}




## ----------------------------------------------------------------------------------------------------------------------

#Function to run landscapemetrics::calculate_lsm and add the layer names
calculate.lsm.with.names <- function(land, ...) {
  out <- land |>
    landscapemetrics::calculate_lsm(...) |>
    dplyr::mutate(layerName = names(land)[layer])
  return(out)
}


#Function to run the landscapemetrics::calculate_lsm function in parallel environment
parallel.calculate.lsm.with.names <- function(land, ...) {
  out <- land |>
    terra::unwrap() |>
    calculate.lsm.with.names(...)
  return(out)
}

#Function to add additional data to the metrics dataframe
#Script-specific, do not port
add.region.to.metric.df <- function(df, nm) {
  #Add region name & level to dataframe
  out <- df |>
    dplyr::mutate(region = nm,
                  epaLevel = dplyr::case_when(region %in% nameJoin$us_l3nameclean ~ "epaL3",
                                              region %in% nameJoin$us_l4l3name ~ "epaL4"))
  return(out)
}


#Function to calculate class-level metrics, add LSM names, and add contextual data over a named list of landscapes
#This function is set up to run in parallel with the futureverse
calculate.class.level.metrics <- function(landList) {
  if(is(future::plan(), "sequential")) {
    print('Calculating class level metrics in sequence')
      out <- landList |>
        purrr::map(calculate.lsm.with.names, #use version that includes raster layer names in output
                   what = c(
                             'lsm_c_ed', #patch area-weighted edge density
                             'lsm_c_ca', #total class area
                             'lsm_c_ai',
                             'lsm_c_clumpy',
                             'lsm_c_cpland',
                             'lsm_c_dcad',
                             'lsm_c_division',
                             'lsm_c_lpi',
                             'lsm_c_lsi',
                             'lsm_c_mesh',
                             'lsm_c_np',
                             'lsm_c_pafrac',
                             'lsm_c_tca',
                             'lsm_c_cohesion'), #aggregation of patches
                   directions = 8,
                   edge_depth = edgeDepth %/% resolution, #without remainder; edge depth is in cells
                   progress = TRUE,
                   full_name = TRUE)
  } else {
    print('Calculating class level metrics in parallel')
      out <- landList |>
        purrr::map(terra::wrap) |> #wrap each raster to send to node
        furrr::future_map(parallel.calculate.lsm.with.names, #use parallelized version that unwraps first
                   what = c(
                             'lsm_c_ed', #patch area-weighted edge density
                             'lsm_c_ca', #total class area
                             'lsm_c_ai',
                             'lsm_c_clumpy',
                             'lsm_c_cpland',
                             'lsm_c_dcad',
                             'lsm_c_division',
                             'lsm_c_lpi',
                             'lsm_c_lsi',
                             'lsm_c_mesh',
                             'lsm_c_np',
                             'lsm_c_pafrac',
                             'lsm_c_tca',
                             'lsm_c_cohesion'), #aggregation of patches
                   directions = 8,
                   edge_depth = edgeDepth %/% resolution, #without remainder; edge depth is in cells
                   progress = TRUE) |>
   purrr::map(dplyr::left_join, y = lsm_abbreviations_names, #NOTE: for some reason cyverse in parallel won't do full_names = TRUE in the function call, so have to do manually
                        by = c("metric", "level"))
  }
  out <- out %>%
    purrr::map2(.f = add.region.to.metric.df, .y = names(.)) #Add more data using add.data.to.metric.df function
  return(out)
}

#Function to calculate class-level metrics, add LSM names, and add contextual data over a named list of landscapes
calculate.patch.level.metrics <- function(landList) {
  if(is(future::plan(), "sequential")) {
    print('Calculating patch level metrics in sequence')
      out <- landList |>
        purrr::map(landscapemetrics::calculate.lsm.with.names,
                   what = "patch", #all patch metrics
                   directions = 8,
                   edge_depth = edgeDepth %/% resolution, #without remainder; edge depth is in cells
                   progress = TRUE,
                   full_name = TRUE)
  } else {
    print('Calculating patch level metrics in parallel')
      out <- landList |>
        purrr::map(terra::wrap) |> #wrap each raster to send to node
        furrr::future_map(parallel.calculate.lsm.with.names, #use parallelized version that unwraps first
                   what = "patch", #all patch metrics
                   directions = 8,
                   edge_depth = edgeDepth %/% resolution, #without remainder; edge depth is in cells
                   progress = TRUE) |>
   purrr::map(dplyr::left_join, y = lsm_abbreviations_names, #NOTE: for some reason cyverse in parallel won't do full_names = TRUE in the function call, so have to do manually
                        by = c("metric", "level"))
  }
  out <- out %>%
    purrr::map2(.f = add.region.to.metric.df, .y = names(.)) #Add more data using add.data.to.metric.df function
  return(out)  
}


#Write out landscape metric CSVs
write.land.metric.csv.w.name <- function(df, nm, dir, set) {
  nmApp <- glue::glue("_{unique(df$level)}_{unique(df$epaLevel)}")
  flNm <- glue::glue("{set}_{nm}{nmApp}.csv")
  readr::write_csv(df, here::here(dir, flNm))
}




# 
# 
# 
# 
# 
# 
# # #Write out landscape metric CSVs
# # write.land.metric.csv.w.name <- function(df, nm, nmApp, dir) {
# #   flNm <- glue::glue("{nm}{nmApp}.csv")
# #   readr::write_csv(df, here::here(dir, flNm))
# # }
# x <- t %>% purrr::map2(.f = add.data.to.metric.df, .y = names(.))
# 
# xx <- tt %>% purrr::map2(.f = add.data.to.metric.df, .y = names(.))
 # xx %>% purrr::map2(.f = write.land.metric.csv.w.name, .y = names(.), dir = outDirT, set = "Fire")



# t <- fireInsectTestList |>
#   purrr::map(landscapemetrics::calculate_lsm,
#              what = c(
#                        'lsm_c_ed', #patch area-weighted edge density
#                        'lsm_c_ai',
#                        'lsm_c_clumpy',
#                        'lsm_c_cpland',
#                        'lsm_c_dcad',
#                        'lsm_c_division',
#                        'lsm_c_lpi',
#                        'lsm_c_lsi',
#                        'lsm_c_mesh',
#                        'lsm_c_np',
#                        'lsm_c_pafrac',
#                        'lsm_c_tca',
#                        'lsm_c_cohesion'), #aggregation of patches
#              directions = 8,
#              progress = TRUE,) |>
#   purrr::map(dplyr::left_join, y = lsm_abbreviations_names,
#                        by = c("metric", "level"))
# 
# tt <- fireTestList |>
#   purrr::map(landscapemetrics::calculate_lsm,
#              what = c(
#                        'lsm_c_ed', #patch area-weighted edge density
#                        'lsm_c_ai',
#                        'lsm_c_clumpy',
#                        'lsm_c_cpland',
#                        'lsm_c_dcad',
#                        'lsm_c_division',
#                        'lsm_c_lpi',
#                        'lsm_c_lsi',
#                        'lsm_c_mesh',
#                        'lsm_c_np',
#                        'lsm_c_pafrac',
#                        'lsm_c_tca',
#                        'lsm_c_cohesion'), #aggregation of patches
#              directions = 8,
#              progress = TRUE,) |>
#   purrr::map(dplyr::left_join, y = lsm_abbreviations_names, #Add full metric names
#                        by = c("metric", "level")) %>%
#   purrr::map2(.f = add.data.to.metric.df, .y = names(.)) #Add more data








## ----------------------------------------------------------------------------------------------------------------------

#A function to read in all csvs output with the same set name
read.set.csvs.as.one <- function(setNm) {
  fileList <- list.files(outDirT, pattern = glue::glue("^{setNm}"), full.names = TRUE)
  csvs <- fileList |> purrr::map(read_csv)
  oneTbl <- dplyr::bind_rows(csvs)
  return(oneTbl)
}




## ----------------------------------------------------------------------------------------------------------------------

#Function to bring all of the above functions together
#This function requires a set name (setNm) as a string, which clarifies what the input raster represents (e.g. "Fire"), and will be used in the output file names
#Output files will be in OutDirT, in the form: "setNm_ecoregion_class/patch_epaLevel.csv"
generate.raster.metrics.for.aoi.set <- function(raster, aoiSet, setNm) {
  if(grepl("_", setNm)) {
    stop("'setNm' cannot contain an underscore")
  }
  #If the aoi set is for epa l4 ecoregions, process appropriately and do both class and patch metrics
  if("us_l4l3name" %in% names(aoiSet)) {
    clippedRasterList <- careful.clip.set(raster, aoiSet, "us_l4l3name")
    classMetrics <- clippedRasterList |>
       calculate.class.level.metrics()
    classMetrics %>%
       purrr::map2(.f = write.land.metric.csv.w.name, .y = names(.), dir = outDirT, set = setNm)
    patchMetrics <- clippedRasterList |>
       calculate.patch.level.metrics()
    patchMetrics %>%
       purrr::map2(.f = write.land.metric.csv.w.name, .y = names(.), dir = outDirT, set = setNm)
  } else { #If L3, only do class metrics
    clippedRasterList <- careful.clip.set(raster, aoiSet, "us_l3nameclean")
    classMetrics <- clippedRasterList |>
       calculate.class.level.metrics()
    classMetrics %>%
       purrr::map2(.f = write.land.metric.csv.w.name, .y = names(.), dir = outDirT, set = setNm)
  }
  gc()
  # asTbl <- read.set.csvs.as.one(setNm)
  # return(asTbl)
}





## ----------------------------------------------------------------------------------------------------------------------


tic("One-layer raster, parallel")
# testOut <- generate.raster.metrics.for.aoi.set(fireInsects, test, setNm = "test_normal")
testOut <- generate.raster.metrics.for.aoi.set(fireInsects, test, setNm = "testSingle")
toc()


tic("Multi-layer raster, parallel")
#testOut <- generate.raster.metrics.for.aoi.set(fireStack, test, setNm = "test")
toc()









# 
# #fireInsectInterestList <- careful.clip.set(fireInsects, aoiL3Interest, us_l3nameclean)
# 
# 
# test.future.function('careful.clip.set', fireInsects, aoiL4Interest[6:8,], "us_l4l3name")
# 
# 
# 
# tic("sequential")
# fireInsectTestList <- careful.clip.set(fireInsects, aoiL4Interest[1:2,], "us_l4l3name")
# toc()
# 
# 
# 
# fireTestList <- careful.clip.set(fireStack, aoiL4Interest[1:2,], "us_l4l3name")
# 
# 
# 
# 
# #Create level 4 class metric list
# classL4 <- fireInsectTestList |>
#   calculate.class.level.metrics()
# # #Write out
# # classL4 %>%
# #   purrr::map2(.f = write.land.metric.csv.w.name, .y = names(.), dir = outDirT)






