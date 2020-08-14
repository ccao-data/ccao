context("test map_kriging()")

##### TEST map_kriging() #####

# Extract test sales data from assessr
sales <- assessr:::sales_prepped %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 3435) %>%
  dplyr::slice(1:300)

# Coerce ccao township boundary to same CRS as sales
bound <- ccao::town_shp %>%
  dplyr::filter(township_name == "Evanston") %>%
  sf::st_transform(3435)

# Run kriging test
kriging_test <- map_kriging(
  sales,
  sale_price,
  boundary = bound,
  cellsize = 1000,
  model = gstat::vgm("Gau")
)

# Run kriging test 2
kriging_test2 <- map_kriging(
  sales,
  sale_price,
  boundary = bound,
  maxdist = 300
)

test_that("output has expected attributes", {
  expect_s3_class(kriging_test, "sf")
  expect_equal(nrow(kriging_test), 222)
  expect_equal(nrow(kriging_test2), 1542)
})

test_that("bad input data stops execution", {
  expect_condition(map_kriging(sales, sale_price, cellsize = "100"))
  expect_error(map_kriging(assessr:::sales_prepped, sale_price))
  expect_error(map_kriging(sales, sale_price, boundary = ccao::town_shp))
  expect_error(map_kriging(sales, sale_price, model = "Gau"))
})
