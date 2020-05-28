library(sf)
library(rmapshaper)
library(magrittr)

# Simplify shapes and then save as .rda
nbhd_shp <- st_read("data-raw/nbhd_shp.geojson") %>%
  ms_simplify(keep = 0.3, keep_shapes = TRUE, snap = TRUE)

usethis::use_data(nbhd_shp, overwrite = TRUE, compress = "xz")
