
library(ecoregions)
library(terra)
library(sf)
library(here)
here::here() #Check here location




devDir <- here::here('data', 'derived')
fireStack <- terra::rast(file.path(devDir, 'fire_dist_stack_west.tif'))
epaL3 <- ecoregions::ContinentalUsEcoregion3 %>% 
  sf::st_transform(terra::crs(fireStack))

sRockies <- epaL3 |>
  dplyr::filter(us_l3name %in% c("Southern Rockies")) |>
  dplyr::group_by(us_l3name) |>
  dplyr::summarize(geometry = st_union(geometry)) |>
  dplyr::mutate(us_l3nameclean = gsub(" ", "", us_l3name))

smallFireStack <- terra::crop(x = fireStack, y = sRockies, mask = TRUE)

terra::writeRaster(smallFireStack,
                   here::here(devDir, "southern_rockies_fire_stack.tif"),
                   overwrite = TRUE,
                   datatype = "INT1U")


tic("TEST with NAs")
tic("zeros")
polygonList <- smallFireStack |>
  terra::subst(c(0), c(NA))
toc()
tic("list")
polygonList <- polygonList |>
  terra::as.list()
toc()
tic("as.polygons")
polygonList <- polygonList |>
  purrr::map(terra::as.polygons)
toc()
tic("as_sf")
polygonList <- polygonList |>
  purrr::map(sf::st_as_sf)
toc()
toc()
polygonList <- polygonList |>
  'names<-'(names(smallFireStack))

polygonList <- polygonList |>
  purrr::map(sf::st_union)


sr2002 <- polygonList[[4]]


#Function to substring from right; ----
#requires string & 'n' characters to keep from right of string
substrRight <- function(str, n) {
  return (substr(str, nchar(str)-n+1, nchar(str)))
}

export.polygons <- function(polygons, name, region) {
  yr <- substrRight(name, 4)
  exportNm <- glue::glue("fire_polygons_{region}_{yr}.gpkg")
  sf::st_write(polygons, here::here('data', 'derived', exportNm))
}

export.polygons(sr2002, name = names(polygonList)[4], region = "test")


polygonList %>% purrr::map2(names(.), .f = export.polygons, region = "southern_rockies")





t <- st_union(sr2002)


#load mtbs & welty
mtbs <- sf::st_read(here::here('data', 'raw', 'mtbs_perimeter_data_1984_2021', 'mtbs_perims_DD.shp')) |>
  sf::st_transform(sf::st_crs(sr2002))
welty <- sf::st_read(here::here('data', 'raw', 'welty_combined_wildland_fire_dataset', 'welty_combined_wildland_fire_perimeters.shp')) |>
  sf::st_transform(sf::st_crs(sr2002))

mtbs2002 <- mtbs |>
  dplyr::mutate(Ig_Year = as.integer(lubridate::year(Ig_Date))) |>
  dplyr::filter(Ig_Year == 2002)
welty2002 <- welty |>
  dplyr::filter(Fire_Year == 2002)

mapview(sRockies, col.regions = "lightgray") +
  mapview(welty2002, col.regions = "green") +
  mapview(sr2002, col.regions = "red") +
  mapview(mtbs2002, col.regions = "blue")
  


data <- sr2002


# Step 1: Split "data" into individual polygons
data_individual <- sf::st_cast(data, "POLYGON")

# Step 2: Identify and merge overlapping polygons
# Spatial join to find overlapping polygons
overlaps <- sf::st_join(data_individual, welty2002, join = st_overlaps, largest = TRUE)






# Step 1: Calculate the length of the sequence needed
num_NA <- sum(is.na(df$OBJECTID))

# Step 2: Create a unique sequence
# Starting point for the sequence
start_seq <- 20021 

# Ensure the sequence does not overlap with existing OBJECTID values
while(any(start_seq:(start_seq + num_NA - 1) %in% df$OBJECTID)) {
  start_seq <- start_seq + num_NA
}

new_ids <- start_seq:(start_seq + num_NA - 1)

# Step 3: Replace NA values in OBJECTID
df <- df %>%
  mutate(OBJECTID = case_when(
    !is.na(OBJECTID) ~ OBJECTID,
    is.na(OBJECTID) ~ as.integer(head(new_ids, 1)), # Assign new ID and remove from the sequence
    new_ids <<- tail(new_ids, -1) # Update the sequence
  ))









# Group by welty2002 ID (or a unique identifier in welty2002) and merge
grouped_overlaps <- overlaps |>
  dplyr::mutate(OJECTID = dplyr::case_when()) |>
  dplyr::group_by(OBJECTID) %>% # Replace 'ID' with the appropriate column name from welty2002
  dplyr::summarise(geometry = st_union(geometry))







mapview(sRockies, col.regions = "lightgray") +
  mapview(welty2002, col.regions = "green") +
  mapview(sr2002, col.regions = "pink") +
  mapview(t, col.regions = "red") +
  mapview(overlaps, col.regions = "purple")




  
