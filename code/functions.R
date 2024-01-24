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