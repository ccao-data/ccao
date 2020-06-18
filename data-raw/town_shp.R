# Load shapes and save them as .rda
town_shp <- sf::st_read("data-raw/town_shp.geojson") %>%
  sf::st_transform(4326) %>%
  mutate_if(is.factor, as.character)

usethis::use_data(town_shp, overwrite = TRUE, compress = "xz")
