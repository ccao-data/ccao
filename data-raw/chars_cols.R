chars_cols <- dplyr::lst(

  # List of columns in dataset that will need to have ADDCHARS values added
  add_target = c(
    "APTS", "ROOMS", "BEDS", "FBATH", "HBATH", "GAR1_SIZE", "BLDG_SF"
  ),
  add_source = c(
    "QU_NUM_APTS", "QU_ROOMS", "QU_BEDS", "QU_FULL_BATH", "QU_HALF_BATH",
    "QU_GARAGE_SIZE", "QU_SQFT_BLD"
  ),

  # List of columns in dataset that will need to have ADDCHARS values replaced
  rep_target = c(
    "TYPE_RESD", "USE", "EXT_WALL", "ROOF_CNST", "BSMT", "BSMT_FIN", "HEAT",
    "AIR", "FRPL", "ATTIC_TYPE", "ATTIC_FNSH", "TP_PLAN", "TP_DSGN",
    "CNST_QLTY", "RENOVATION", "SITE", "GAR1_CNST", "GAR1_ATT", "GAR1_AREA",
    "PORCH", "REPAIR_CND", "CLASS"
  ),

  rep_source = c(
    "QU_TYPE_OF_RES", "QU_USE", "QU_EXTERIOR_WALL", "QU_ROOF",
    "QU_BASEMENT_TYPE", "QU_BASEMENT_FINISH", "QU_HEAT", "QU_AIR",
    "QU_FIRE_PLACE", "QU_ATTIC_TYPE", "QU_ATTIC_FINISH", "QU_TYPE_PLAN",
    "QU_TYPE_DESIGN", "QU_CONSTRUCT_QUALITY", "QU_RENOVATION", "QU_SITE_DESIRE",
    "QU_GARAGE_CONST", "QU_GARAGE_ATTACHED", "QU_GARAGE_AREA", "QU_PORCH",
    "QU_STATE_OF_REPAIR", "QU_CLASS"
  )
)

# Save data to .rda for internal use only
usethis::use_data(chars_cols, overwrite = TRUE)
