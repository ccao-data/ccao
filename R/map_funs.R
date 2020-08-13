#' Create smooth interpolated maps with kriging
#'
#' @description Kriging is a way to interpolate intermediate values using a
#'   set of points. This function uses kriging to create simple maps using
#'   a single numeric vector input and a boundary file. The output is a set of
#'   grid squares within the boundary, where each square has an interpolated
#'   value.
#'
#' @param data An \code{sf} data frame containing POINT data, preferably in
#'   a planar, meters-based projection e.g. for Illinois, use 3435.
#' @param col A column to use as the value in the kriging formula \code{y ~ 1}.
#'   Usually sale price or assessed value.
#' @param boundary An \code{sf} data frame containing a POLYGON or MULTIPOLYGON
#'   geometries. These will be merged and then use to determine which grid
#'   squares to keep (only those within the boundary will remain). Uses the
#'   convex hull of \code{data} if \code{NULL}.
#' @param model A \code{gstat::vgm()} object passed to
#'   \code{gstat::fit.variogram()}. Uses "Gau" if \code{NULL},
#' @param cellsize Numeric value in the same projection units as \code{data}.
#'   Determines the size of grid cell to create and then use for kriging. For
#'   example, if the CRS of \code{data} is in meters, then choosing 100 will
#'   create 100 meter wide cells. Creates a 50x50 grid if \code{NULL}.
#'
#' @return An \code{sf} data frame containing 1 row for each grid cell created
#'   according to \code{cellsize}, excluding cells that do not intersect with
#'   \code{boundary}. The predicted value is the result of the kriging model.
#' 
#' @importFrom dplyr distinct
#' @export
map_kriging <- function(data, col, boundary = NULL, model = NULL, cellsize = NULL) { # nolint

  # Extract the selection variable from the col argument
  selected_var <- tidyselect::vars_select(names(data), {{ col }})

  # Input checking and error handling
  stopifnot(
    sf::st_crs(data) == sf::st_crs(boundary) | is.null(boundary),
    length(selected_var) == 1
  )
  formula <- stats::as.formula(paste(selected_var, "~ 1"))

  # Jitter points to fix errors in overlapping observations in variogram
  data <- data %>%
    sf::st_jitter() %>%
    dplyr::distinct({{ col }})

  # Create variogram of values with location data
  loc_vgm <- gstat::variogram(formula, data)

  # Fit the variogram using the specified model
  if (is.null(model)) model <- gstat::vgm("Gau")
  loc_fit_sph <- gstat::fit.variogram(loc_vgm, model = model)

  # Merge multiple polygons into single boundary, or get convex hull
  if (is.null(boundary)) {
    boundary <- sf::st_convex_hull(sf::st_union(data))
  } else {
    boundary <- sf::st_union(boundary)
  }

  # Create spatial grid object for kriging
  if (is.null(cellsize)) {
    grid <- sf::st_make_grid(boundary, n = 50)
  } else {
    grid <- sf::st_make_grid(boundary, cellsize = cellsize)
  }

  # Perform kriging on the dataset with its fitted variogram
  value_kriged <- gstat::krige(
    formula = formula,
    locations = data,
    newdata = grid,
    model = loc_fit_sph
  )

  # Filter grid so that it conforms to the boundaries of shapefile
  grid_in_bound <- sf::st_intersects(value_kriged, boundary, sparse = FALSE)
  grid_value_kriged <- value_kriged[grid_in_bound, ]

  return(grid_value_kriged)
}
