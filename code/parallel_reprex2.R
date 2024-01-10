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
  dplyr::filter(us_l4name == "Foothill Potholes" | us_l4name == "Western Beaverhead Mountains") %>%
  split(f=.[["us_l4name"]])

testAOIsWrap <- testAOIs |> 
  purrr::map(terra::vect) |>
  purrr::map(terra::wrap)

fireInsectsWrap <- fireInsects |> terra::wrap()
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




