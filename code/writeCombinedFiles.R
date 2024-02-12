

beetle5yr <- terra::rast(beetle5yrF)

terra::writeRaster(beetle5yr,
                   here::here("data/derived/beetle_totals_all.tif"),
                   overwrite = TRUE,
                   datatype = 'INT1U')
