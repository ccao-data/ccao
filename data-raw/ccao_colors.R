
# Generate named list of colors
ccao_colors <- list(
  navy       = "#29428D",
  gold       = "#FFAF00",
  buttermilk = "#FFDF99",
  parchment  = "#FFFBF3",
  brown      = "#954B34",
  darkbrown  = "#161211",
  lightblue  = "#00AFF0",
  lightgreen = "#81CA9C",
  green      = "#006F45",
  white      = "#FFFFFF",
  grey       = "#787878",
  black      = "#000000"
)

# Save data to .rda
usethis::use_data(ccao_colors, overwrite = TRUE)
