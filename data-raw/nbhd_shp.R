# Load shapes and save them as .rda
nbhd_shp <- sf::st_read("data-raw/nbhd_shp.geojson")

usethis::use_data(nbhd_shp, overwrite = TRUE, compress = "xz")
