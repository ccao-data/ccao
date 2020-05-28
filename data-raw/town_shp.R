library(sf)
library(rmapshaper)
library(magrittr)

# Simplify shapes and then save as .rda
town_shp <- st_read("data-raw/town_shp.geojson") %>%
  ms_simplify(keep = 0.3, keep_shapes = TRUE, snap = TRUE)

usethis::use_data(town_shp, overwrite = TRUE, compress = "xz")
