# A script for functions called throughout this project.
# Sourced by other scripts
# 
# Tyler L. McIntosh
# CU Boulder CIRES Earth Lab
# Last updated: 1/24/24
# 
# This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

# FUNCTIONS ----

## GENERAL USE FUNCTIONS ----

# A function to check and install packages provided to the function. Written in part by ChatGPT4.
# PARAMETERS
# packageList : a vector of packages used in the script, e.g. c("here", "dplyr")
# autoInstall : either "y" or "n" (default). "y" will install all required packages without asking the user for permission. "n" will ask permission from the user.
install.and.load.packages <- function(packageList, autoInstall = "n") {
  missingCranPackages <- c()
  missingGithubPackages <- c()
  for (package in packageList) {
    packageLoaded <- FALSE
    # Check if the package is from GitHub
    if (grepl("/", package)) {
      packageName <- unlist(strsplit(package, "/"))[2]
      packageLoaded <- require(packageName, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    } else {
      packageLoaded <- require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    }
    # Mark as missing if not loaded
    if (!packageLoaded) {
      if (grepl("/", package)) {
        missingGithubPackages <- c(missingGithubPackages, package)
      } else {
        missingCranPackages <- c(missingCranPackages, package)
      }
    }
  }
  # Install missing CRAN packages
  if (length(missingCranPackages) > 0) {
    cat("The following CRAN packages are missing: ", paste(missingCranPackages, collapse = ", "), "\n")
    if(autoInstall == "y") {
      response <- "y"
      cat("Installing the missing packages\n")
    } else {
      response <- readline(prompt = "\nDo you want to install the missing CRAN packages? (y/n): ")
    }
    if (tolower(response) == "y") {
      install.packages(missingCranPackages)
      for (package in missingCranPackages) {
        require(package, character.only = TRUE)
      }
    } else {
      cat("Skipping installation of missing CRAN packages.\n")
    }
  }
  # Ask to install the 'remotes' package if GitHub packages are missing and 'remotes' is not installed
  if (length(missingGithubPackages) > 0 && !requireNamespace("remotes", quietly = TRUE)) {
    if(autoInstall == "y") {
      response <- "y"
      cat("Installing 'remotes' package to install GitHub packages\n")
    } else {
      response <- readline(prompt = "\nDo you want to install the 'remotes' package? (It is required to install packages from GitHub) (y/n): ")
    }
    if (tolower(response) == "y") {
      install.packages("remotes")
    } else {
      cat("Skipping installation of GitHub packages.\n")
      missingGithubPackages <- c() # Clear the list of GitHub packages
    }
  }
  # Install missing GitHub packages
  if (length(missingGithubPackages) > 0) {
    cat("The following GitHub packages are missing: ", paste(missingGithubPackages, collapse = ", "), "\n")
    if(autoInstall == "y") {
      response <- "y"
      cat("Installing the missing packages\n")
    } else {
      response <- readline(prompt = "\nDo you want to install the missing GitHub packages? (y/n): ")
    }    
    if (tolower(response) == "y") {
      for (package in missingGithubPackages) {
        remotes::install_github(package)
        packageName <- unlist(strsplit(package, "/"))[2]
        require(packageName, character.only = TRUE)
      }
    } else {
      cat("Skipping installation of missing GitHub packages.\n")
    }
  }
  cat("All specified packages installed and loaded.\n")
}

# This function takes in a set of polygons and returns the same set of polygons
# with the area of the polygon added as a column called "stArea", in whatever units the polygon CRS is in
# PARAMETERS
# polys : a set of polygons as an sf object
st.area.to.poly <- function(polys) {
  
  out <- polys |>
    sf::st_area() |> #get area from sf package
    units::drop_units() %>%
    cbind(polys, .) |> #join to polygons
    dplyr::rename(stArea = `.`) #rename
  
  return(out)
  
}

# Function to substring from right
# PARAMETERS 
# str : a string
# n : the number of characters to keep from right of string, as an integer
substrRight <- function(str, n) {
  
  return (substr(str, nchar(str)-n+1, nchar(str)))
  
}


# A function to test a function that has been set up to run in either sequence or parallel.
# For use in troubleshooting functions written to parallelize using the futureverse, depending on what plan() has been set to,
# e.g. with   if (is(future::plan() ,"sequential")) vs "multisession"
# This function will reset future::plan() to the state before the function was run
# PARAMETERS
# fun = name of function as a string, e.g. "calculate.class.level.metrics"
# ... = all parameters for running fun
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

#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped to vector
# PARAMETERS
# raster : a SpatRaster or PackedSpatRaster object
# vector : a SpatVector, PackedSpatVector or SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
careful.clip <- function(raster, vector, mask) {
  pack <- FALSE
  
  #Unpack if parallelized inputs
  if(class(raster)[1] == "PackedSpatRaster") {
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if(class(vector)[1] == "PackedSpatVector") {
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  #Handle unpacked spatVector
  if(class(vector)[1] == "SpatVector") {
    vector <- sf::st_as_sf(vector)
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
                   mask = mask) #crop & mask
  
  #Repack if was packed coming in (i.e. parallelized)
  if(pack) {
    r <- terra::wrap(r)
  }
  return(r)
}


#Function to clip a raster to a set of polygons (one clip per polygon in set)
#Returns a set of clipped rasters as a named list
#Works in either parallel or sequence, depending on what future::plan() has been set to
# PARAMETERS
# raster : a SpatRaster object
# vector : an SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
# namefield: indicates the vector field to use to name the item in the returned list (e.g. "US_L4L3NAMECLEAN")
careful.clip.set <- function(raster, vectors, namefield, mask) {
  #Split vectors into a list
  if(nrow(vectors) == 1) {
    splitVec <- list()
    splitVec[[1]] <- vectors
  } else {
    splitVec <- split(vectors, f=vectors[[namefield]])
  }
  #Map over list of vectors and perform clip using careful.clip
  if (is(future::plan() ,"sequential")) {
    print("Performing clip set in sequence")
    out <- splitVec |> purrr::map(careful.clip, raster = raster, mask = TRUE)
  } else { #Pack & parallelize
    print("Performing clip set in parallel")
    r <- terra::wrap(raster, proxy = TRUE)
    v <- splitVec |> 
      purrr::map(terra::vect) |> 
      purrr::map(terra::wrap)
    out <- v |> furrr::future_map(careful.clip, raster = r, mask = TRUE)
    out <- out |>
      purrr::map(terra::unwrap)
  }
  return(out)
}

#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped to vector
# PARAMETERS
# raster : a SpatRaster or PackedSpatRaster object
# vector : a SpatVector, PackedSpatVector or SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
careful.clip2 <- function(raster, vector, mask) {
  pack <- FALSE
  
  #Unpack if parallelized inputs
  if(class(raster)[1] == "PackedSpatRaster") {
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if(class(vector)[1] == "PackedSpatVector") {
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  #Create spatraster if filename
  if(class(raster)[1] == "character") {
    raster <- terra::rast(raster)
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



#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped to vector
# PARAMETERS
# raster : a SpatRaster or PackedSpatRaster object
# vector : a SpatVector, PackedSpatVector or SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
careful.clip.universal <- function(raster, vector, mask) {
  pack <- FALSE
  
  #Unpack if parallelized inputs
  if(class(raster)[1] == "PackedSpatRaster") {
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if(class(vector)[1] == "PackedSpatVector") {
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  #Handle unpacked spatVector
  if(class(vector)[1] == "SpatVector") {
    vector <- sf::st_as_sf(vector)
  }
  
  #If using raster package
  if(class(raster)[1] == "RasterLayer" | class(raster)[1] == "RasterStack" | class(raster)[1] == "RasterBrick") {
    
    #Perform operation
    if (sf::st_crs(vector) != raster::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
      print("Projecting vector")
      vector <- sf::st_transform(vector, raster::crs(raster)) 
    } else {
      print("Vector already in raster CRS")
    }
    print("Clipping")
    r <- raster::crop(raster,
                      vector)
    if(mask) {
      r <- r |> raster::mask(vector)
    }
    
    return(r)
    
  } else { #terra package
    
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
                     mask = mask) #crop & mask
    
    #Repack if was packed coming in (i.e. parallelized)
    if(pack) {
      r <- terra::wrap(r)
    }
    return(r)
    
  }
}



#Function to clip a raster to a set of polygons (one clip per polygon in set)
#Returns a set of clipped rasters as a named list
#Works in either parallel or sequence, depending on what future::plan() has been set to
# PARAMETERS
# raster : a SpatRaster object
# vector : an SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
# namefield: indicates the vector field to use to name the item in the returned list (e.g. "US_L4L3NAMECLEAN")
careful.clip.set2 <- function(raster, vectors, namefield, mask) {
  #Split vectors into a list
  if(nrow(vectors) == 1) {
    splitVec <- list()
    splitVec[[1]] <- vectors
  } else {
    splitVec <- split(vectors, f=vectors[[namefield]])
  }
  #Map over list of vectors and perform clip using careful.clip
  if (is(future::plan() ,"sequential")) {
    print("Performing clip set in sequence")
    out <- splitVec |> purrr::map(careful.clip2, raster = raster, mask = TRUE)
  } else { #Pack & parallelize
    print("Performing clip set in parallel")
    if(class(raster)[1] == "character") {
      r <- raster
    } else {
      r <- terra::wrap(raster, proxy = TRUE)
    }
    v <- splitVec |> 
      purrr::map(terra::vect) |> 
      purrr::map(terra::wrap)
    out <- v |> furrr::future_map(careful.clip2, raster = r, mask = TRUE)
    out <- out |>
      purrr::map(terra::unwrap)
  }
  return(out)
}


# A function to take in an sf object with multiple polygon rows and split it into a list of polygons.
# The split will be performed based on namefield
# PARAMETERS
# vector : SF object with 1 or more polygons (rows)
# namefield : string containing the name of the variable to use for splitting (e.g. "AreaName")
sf.to.polygon.list <- function(vectors, namefield) {
  if(nrow(vectors) == 1) {
    splitVec <- list()
    splitVec[[1]] <- vectors
  } else {
    splitVec <- split(vectors, f=vectors[[namefield]])
  }
  
  return(splitVec)
}

# Function to run landscapemetrics::calculate_lsm and add the raster layer names for multi-band rasters
# PARAMETERS
# land : a landscape in the form of a spatRaster
calculate.lsm.with.names <- function(land, ...) {
  out <- land |>
    landscapemetrics::calculate_lsm(...) |>
    dplyr::mutate(layerName = names(land)[layer])
  return(out)
}

# Function to test if an object can be serialized
# PARAMETERS
# obj : any object
test.serialization <- function(obj) {
  
  serialized_obj <- serialize(obj, NULL)
  tryCatch({
    serialize(obj, NULL)
    print("Serialized successfully")
  }, error = function(e) {
    cat("Can't serialize: ", e$message, "\n")
  })
  
  tryCatch({
    unserialize(serialized_obj)
    print("Unserialized successfully")
  }, error = function(e) {
    cat("Can't unserialize: ", e$message, "\n")
  })
}

## PROJECT-SPECIFIC FUNCTIONS ----

