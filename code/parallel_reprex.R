library(furrr)
library(ecoregions)
library(terra)
library(sf)

nCores <- 2
options(future.seed = TRUE)
future::plan("multisession", workers = nCores)

#Load data
fireInsects <- terra::rast("~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/derived/fire_and_insects.tif")
epaL4 <- ecoregions::ContinentalUsEcoregion4 |> 
  sf::st_transform(terra::crs(fireInsects))

#Two small l4 ecoregions to test on
testAOIs <- epaL4 |>
  dplyr::group_by(us_l4name) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::filter(us_l4name == "Foothill Potholes" | us_l4name == "Western Beaverhead Mountains")


#Function to clip a raster to a set of polygons (one clip per polygon in set), in either sequence or parallel
#Namefield indicates the field to use to name the item in the returned list (e.g. "us_l4l3name")
#Returns the set of rasters as a named list 
careful.clip.set <- function(raster, vectors, namefield) {
  splitVec <- split(vectors, f=vectors[[namefield]])
  if (is(future::plan() ,"sequential")) {
    print("Performing clip set in sequence")
    out <- splitVec |> purrr::map(careful.clip, raster = raster)
  } else { #Pack & parallelize
    print("Performing clip set in parallel")
    r <- terra::wrap(raster)
    v <- splitVec |> 
      purrr::map(terra::vect) |> 
      purrr::map(terra::wrap)
    out <- v |> furrr::future_map(careful.clip, raster = r, .options = furrr_options(seed = TRUE))
    out <- out |>
      purrr::map(terra::unwrap)
  }
  return(out)
}

#Function to clip a raster to a vector, ensuring in same projection, either from a sequence or parallel operation
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



#######################################
#EXAMPLE 1
# These functions work when run in sequence and parallel locally, but line 104 terminates the R instance on CyVerse and creates a file names "core.###" (with actual numbers, different every time)

future::plan("sequential")

clippedSeq <- testAOIs %>% careful.clip.set(raster = fireInsects, vectors = ., namefield = "us_l4name")

# future::plan("multisession", workers = nCores)
# 
# clippedPar <- testAOIs %>% careful.clip.set(raster = fireInsects, vectors = ., namefield = "us_l4name")








#######################################
#EXAMPLE 2

parallel.freq <- function(land) {
  land <- terra::unwrap(land)
  return(freq(land))
}

freqsS <- clippedSeq |> purrr::map(terra::freq)

future::plan("multisession", workers = nCores)

clippedSeqWrap <- clippedSeq |> purrr::map(terra::wrap)
freqsP <- clippedSeqWrap |> furrr::future_map(parallel.freq)

freqsS
freqsP











##########################################



library(magrittr)
library(furrr)
library(tictoc)
plan(multisession, workers = 2)

#This works
1:10 %>%
  future_map(rnorm, n = 10, .options = furrr_options(seed = 123)) %>%
  future_map_dbl(mean)

#########TERRA FREQ PARALLEL REPREX

library(terra)
pathraster <- system.file("ex/elev.tif", package = "terra")
r <- rast(pathraster)
r2 <- r * 2
plot(r)
plot(r2)
l <- list(r, r2)

tic()
freqsS <- l |> purrr::map(terra::freq)
toc()

parallel.freq <- function(land) {
  land <- terra::unwrap(land)
  return(freq(land))
}

tic()
lwrapped <- l |> purrr::map(terra::wrap)
freqsP <- lwrapped |> furrr::future_map(parallel.freq)
toc()


############### PARALLEL CROP REPREX

parallel.crop <- function(x, y, ...) {
  x <- terra::unwrap(x)
  y <- terra::unwrap(y)
  cropped <- terra::crop(x, y , ...) 
  return(terra::wrap(cropped))
}


pathvect <- system.file("ex/lux.shp", package = "terra")
v <- sf::st_as_sf(terra::vect(pathvect))

xxx <- v %>% careful.clip.set(raster = r, vectors = ., namefield = "NAME_2")

plot(xxx[[1]])
