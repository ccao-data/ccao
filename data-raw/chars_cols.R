chars_cols <- dplyr::lst(

  # List of columns in data set that will need to have ADDCHARS values added
  add = list(
    target = c(
      "char_rooms", "char_beds", "char_fbath", "char_hbath", "char_bldg_sf",
      "char_land_sf", "char_ncu"
    ),
    source = c(
      "qu_rooms", "qu_beds", "qu_full_bath", "qu_half_bath", "qu_sqft_bld",
      "qu_lnd_sqft", "qu_no_com_unit"
    )
  ),

  # List of columns in data set that will need to have ADDCHARS values replaced
  replace = list(
    target = c(
      "char_type_resd", "char_use", "char_ext_wall", "char_roof_cnst",
      "char_bsmt", "char_bsmt_fin", "char_heat","char_air", "char_frpl",
      "char_attic_type", "char_attic_fnsh", "char_tp_plan", "char_tp_dsgn",
      "char_cnst_qlty", "char_renovation", "char_site", "char_gar1_cnst",
      "char_gar1_att", "char_gar1_area", "char_porch", "char_repair_cnd",
      "char_gar1_size", "char_apts", "meta_class"
    ),
    source = c(
      "qu_type_of_res", "qu_use", "qu_exterior_wall", "qu_roof",
      "qu_basement_type", "qu_basement_finish", "qu_heat", "qu_air",
      "qu_fire_place", "qu_attic_type", "qu_attic_finish", "qu_type_plan",
      "qu_type_design", "qu_construct_quality", "qu_renovation", "qu_site_desire",
      "qu_garage_const", "qu_garage_attached", "qu_garage_area", "qu_porch",
      "qu_state_of_repair", "qu_garage_size", "qu_num_apts", "qu_class"
    )
  )
)

# Save data to .rda for internal use only
usethis::use_data(chars_cols, overwrite = TRUE)
