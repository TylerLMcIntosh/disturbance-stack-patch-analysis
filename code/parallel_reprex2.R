library(furrr)
library(terra)
library(sf)
library(dplyr)

nCores <- 2
options(future.seed = TRUE)
future::plan("multisession", workers = nCores)

#Load data
fireInsects <- terra::rast("~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/derived/fire_and_insects.tif")
epaL4 <- sf::st_read("~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/raw/us_eco_l4/us_eco_l4_no_st.shp") |> 
  sf::st_transform(terra::crs(fireInsects))

#Two small l4 ecoregions to test on
testAOIs <- epaL4 |>
  dplyr::group_by(US_L4NAME) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::filter(US_L4NAME == "Foothill Potholes" | US_L4NAME == "Western Beaverhead Mountains") %>%
  split(f=.[["US_L4NAME"]])

testAOIsWrap <- testAOIs |> 
  purrr::map(terra::vect) |>
  purrr::map(terra::wrap)

fireInsectsWrap <- fireInsects |> terra::wrap() #THIS CAUSES THE CRASH ON BOTH RSTUDIOROCKER & MACROCONTAINER
fireStackWrap <- fireStack |> terra::wrap()

parallel.crop <- function(x, y, ...) {
  x <- terra::unwrap(x)
  y <- terra::unwrap(y)
  cropped <- terra::crop(x, y , ...) 
  return(terra::wrap(cropped))
}

croppedPwrapped <- testAOIsWrap %>% furrr::future_map(.f = function(aoi){parallel.crop(fireInsectsWrap, aoi, mask = TRUE)}, .options = furrr_options(seed = TRUE))
croppedP <- croppedPwrapped |>
  purrr::map(terra::unwrap)

croppedPwrapped2 <- testAOIsWrap %>% furrr::future_map(.f = function(aoi){parallel.crop(fireStackWrap, aoi, mask = TRUE)}, .options = furrr_options(seed = TRUE))
croppedP2 <- croppedPwrapped2 |>
  purrr::map(terra::unwrap)

plot(croppedP2[[1]])


#ALL OF THE BELOW WORKS ON: ROCKERRSTUDIO (NEED TO TEST ON MACROSYSTEMS-CONTAINER)

#WITHOUT WRAPPING
crop.with.names.no.wrap <- function(rastF, aoiF, aoiRegionNm, ...) {
  r <- terra::rast(rastF)
  aoiFull <- sf::st_read(aoiF)
  aoi <- aoiFull |>
    dplyr::group_by(US_L4NAME) |>
    dplyr::summarize(geometry = st_union(geometry)) |>
    dplyr::filter(US_L4NAME == aoiRegionNm)
  cropped <- terra::crop(r, aoi , ...)
  return(terra::freq(cropped))
}

#WITH WRAP OF SMALL RASTER AT END
crop.with.names.no.wrap2 <- function(rastF, aoiF, aoiRegionNm, ...) {
  r <- terra::rast(rastF)
  aoiFull <- sf::st_read(aoiF)
  aoi <- aoiFull |>
    dplyr::group_by(US_L4NAME) |>
    dplyr::summarize(geometry = st_union(geometry)) |>
    dplyr::filter(US_L4NAME == aoiRegionNm)
  cropped <- terra::crop(r, aoi , ...)
  return(terra::wrap(cropped))
}

x <- c("Foothill Potholes", "Western Beaverhead Mountains") %>%
  furrr::future_map(.f = function(aoiNm) {crop.with.names.no.wrap2(
    rastF = "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/derived/fire_and_insects.tif",
    aoiF = "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack-patch-analysis/data/raw/us_eco_l4/us_eco_l4_no_st.shp",
    aoiRegionNm = aoiNm,
    mask = TRUE)}, .options = furrr_options(seed = TRUE))

t <- x[[1]] |> terra::unwrap()
