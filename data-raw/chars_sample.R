library(DBI)
library(noctua)
library(readr)

aws_athena_conn <- DBI::dbConnect(noctua::athena())

# Get sample of Athena residential universe
chars_sample_athena <- dbGetQuery(
  aws_athena_conn,
  "
  SELECT
    pin,
    year,
    class,
    char_yrblt,
    char_bldg_sf,
    char_land_sf,
    char_beds,
    char_rooms,
    char_fbath,
    char_hbath,
    char_frpl,
    char_type_resd,
    char_cnst_qlty,
    char_apts,
    char_tp_dsgn,
    char_attic_fnsh,
    char_gar1_att,
    char_gar1_area,
    char_gar1_size,
    char_gar1_cnst,
    char_attic_type,
    char_bsmt,
    char_ext_wall,
    char_heat,
    char_repair_cnd,
    char_bsmt_fin,
    char_roof_cnst,
    char_use,
    char_age,
    char_site,
    char_ncu,
    char_renovation,
    char_porch,
    char_air,
    char_tp_plan
  FROM default.vw_card_res_char
  WHERE pin IN (
    '09254040180000', '09361030150000', '09363230550000', '10253190450000',
    '10254170360000', '13253160160000', '13253230040000', '13362270230000',
    '14321260280000', '17032010190000'
  )
  AND year >= '2015'
  "
)
usethis::use_data(chars_sample_athena, overwrite = TRUE)

# Get sample ADDCHARS data
chars_sample_hie <- dbGetQuery(
  aws_athena_conn,
  "
  SELECT *
  FROM ccao.hie
  WHERE pin IN (
    '09254040180000', '09361030150000', '09363230550000', '10253190450000',
    '10254170360000', '13253160160000', '13253230040000', '13362270230000',
    '14321260280000', '17032010190000'
  )
  "
)
usethis::use_data(chars_sample_hie, overwrite = TRUE)
