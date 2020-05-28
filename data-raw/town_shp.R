# Load shapes and save them as .rda
town_shp <- sf::st_read("data-raw/town_shp.geojson")

usethis::use_data(town_shp, overwrite = TRUE, compress = "xz")