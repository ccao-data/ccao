# Load shapes and save them as .rda
nbhd_shp <- sf::st_read("data-raw/nbhd_shp.geojson") %>%
  sf::st_transform(4326) %>%
  mutate_if(is.factor, as.character)

usethis::use_data(nbhd_shp, overwrite = TRUE, compress = "xz")
