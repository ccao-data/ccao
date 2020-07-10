
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CCAO Package <a href='https:/glue.tidyverse.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

A package to manage, distribute, and version control *CCAO-specific*
functions. These functions are used throughout CCAO applications,
models, and diagnostics. For generalized versions of assessment-related
functions, see
[assessR](https://gitlab.com/ccao-data-science---modeling/packages/assessr).

## Installation

You can install the released version of `ccao` directly from GitLab by
running the following R command after installing `remotes`:

``` r
remotes::install_gitlab("ccao-data-science---modeling/packages/ccao")
```

Once it is installed, you can use it just like any other package. Simply
call `library(ccao)` at the beginning of your script.

## Included Functions and Data

##### Functions currently included in `ccao`:

  - `ccao_cod()`, `ccao_prd()`, `ccao_prb()` calculate assessment
    performance statistics in accordance with CCAO Data Science
    Department’s [Standard Operating Procedure on Sales Ratio
    Studies](https://gitlab.com/ccao-data-science---modeling/documentation/wiki_content/-/blob/f1efdcf7b66ab238efa438ee2f35e659222e76af/sops/sales-ratio-studies.md).
    The main changes are enforcement of outlier trimming (top and bottom
    5% of ratios are always dropped) and a strict minimum sample size (N
    must be \>= 30). Each function outputs a named list containing the
    statistic, its confidence interval, whether or not the IAAO standard
    was met, and the number of observations used to calculate the stat.

-----

  - `pin_format_pretty()` adds dash separators to input PINs to make
    them human-readable
  - `pin_clean()` removes separators and whitespace from input PINs and
    warns if a PIN is invalid

-----

  - `town_convert()` converts from township name to township number and
    visa versa
  - `town_get_triad()` returns the triad code or name of the input
    town(s)
  - `town_get_assmnt_year()` returns assessment year nearest to the
    `year` argument for a given input township

-----

  - `format_as400()` formats predicted values in the specification
    necessary to upload to the AS/400

##### Data currently included in `ccao`:

  - `appeal_dict` is a dictionary of appeal reason codes used in CCAO
    internal systems.
  - `ccao_colors` is a named list of CCAO Comms Department colors, see
    below for palette
  - `cdu_dict` is a dictionary of CCAO condition-desirability-utility
    (CDU) codes. These codes are usually used to represent different
    incentive conditions
  - `chars_dict` is a crosswalk of human-readable translations of CCAO
    database characteristic codes
  - `class_dict` is a dictionary of property class numeric codes and
    their human-readable equivalent
  - `nbhd_recodes` contains recodes for individual neighborhoods
  - `nbhd_shp` is a spatial (sf) data frame containing boundaries of
    CCAO neighborhoods<sup>1</sup>
  - `town_dict` is a crosswalk of township names, their equivalent
    numeric codes, and the triad they are in
  - `town_shp` is a spatial (sf) data frame containing boundaries of
    Cook County townships<sup>1</sup>

<sup>1</sup> :warning: The `sf` library **must** be loaded first in
order to load this data. If you encounter the error `C stack usage
{number} is too close to the limit` when loading the data, update your
version of `sf`

## Common Spatial Boundaries

This package contains spatial data frames representing CCAO
administrative boundaries. Note that you **must have the `sf` package
installed and loaded** in order to correctly load spatial data frames.

``` r
library(sf)
library(ccao)

# Plot township boundaries alone
plot(ccao::town_shp[1], main = "Township Boundaries")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
# Plot township boundaries with neighborhoods
plot(ccao::nbhd_shp[1], main = "Townships with Neighborhoods")
```

<img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

## Handling for 288s (Home Improvement Exemptions)

``` r
library(dplyr)
library(tidyr)
library(knitr)

# Choose a random sample PIN from the sample ADDCHARS data
sample_chars <- ccao::chars_sample_addchars %>%
  filter(QU_PIN == "05342230050000")

# This PIN has an increase in square footage and added a garage over two
# separate years
sample_chars %>%
  select(QU_PIN, TAX_YEAR, QU_TOWN, QU_GARAGE_ATTACHED, QU_SQFT_BLD) %>%
  kable(format = "markdown", digits = 3)
```

| QU\_PIN        | TAX\_YEAR | QU\_TOWN | QU\_GARAGE\_ATTACHED | QU\_SQFT\_BLD |
| :------------- | --------: | -------: | -------------------: | ------------: |
| 05342230050000 |      2014 |       23 |                    0 |           135 |
| 05342230050000 |      2015 |       23 |                    2 |             0 |

``` r

# Sparsify the data. Both of these 288s end in 2018, but one starts in 2014 
# and one starts in 2015
sparse_chars <- sample_chars %>%
  ccao::chars_sparsify(
    pin_col = QU_PIN,
    year_col = TAX_YEAR, 
    town_col = as.character(QU_TOWN),
    upload_date_col = QU_UPLOAD_DATE,
    additive_cols = any_of(ccao:::chars_get_type("ADD", "col_addchars")),
    replacement_cols = any_of(ccao:::chars_get_type("REPLACE", "col_addchars"))
  ) %>%
  mutate(QU_CLASS = substr(QU_CLASS, 1, 3))

# The resulting sparse data can be joined onto any data containing CCAOSFCHARS
sparse_chars %>%
  select(QU_PIN, YEAR, QU_GARAGE_ATTACHED, QU_SQFT_BLD) %>%
  kable(format = "markdown", digits = 3)
```

| QU\_PIN        | YEAR | QU\_GARAGE\_ATTACHED | QU\_SQFT\_BLD |
| :------------- | ---: | -------------------: | ------------: |
| 05342230050000 | 2014 |                    0 |           135 |
| 05342230050000 | 2015 |                    2 |           135 |
| 05342230050000 | 2016 |                    2 |           135 |
| 05342230050000 | 2017 |                    2 |           135 |
| 05342230050000 | 2018 |                    2 |           135 |

``` r

# Here is an example data frame where the sparse data is merged onto
# characteristic data and the characteristics are then updated using mutate()
updated_chars <- chars_sample_universe %>%
  filter(PIN %in% sample_chars$QU_PIN) %>%
  left_join(sparse_chars, by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR")) %>%
  arrange(PIN, TAX_YEAR) %>%
  rowwise() %>%
  mutate(
    across(
      any_of(ccao:::chars_get_type("ADD", "col_data")),
      function(x, y = cur_column()) sum(x, get(ccao:::chars_get_col(y)), na.rm = T)
    ),
    across(
      any_of(ccao:::chars_get_type("REPLACE", "col_data")),
      function(x, y = cur_column()) replace_na(get(ccao:::chars_get_col(y)), x)
    )
  )

# Show updated characteristics vs ADDCHARS
updated_chars %>%
  mutate(ACTIVE_288 = !is.na(QU_SQFT_BLD)) %>%
  select(
    PIN, ACTIVE_288, TAX_YEAR, GAR1_ATT,
    BLDG_SF, QU_GARAGE_ATTACHED, QU_SQFT_BLD
  ) %>%
  kable(format = "markdown", digits = 3)
```

| PIN            | ACTIVE\_288 | TAX\_YEAR | GAR1\_ATT | BLDG\_SF | QU\_GARAGE\_ATTACHED | QU\_SQFT\_BLD |
| :------------- | :---------- | --------: | --------: | -------: | -------------------: | ------------: |
| 05342230050000 | FALSE       |      2013 |         0 |     2478 |                   NA |            NA |
| 05342230050000 | TRUE        |      2014 |         0 |     2613 |                    0 |           135 |
| 05342230050000 | TRUE        |      2015 |         2 |     2613 |                    2 |           135 |
| 05342230050000 | TRUE        |      2016 |         2 |     2613 |                    2 |           135 |
| 05342230050000 | TRUE        |      2017 |         2 |      135 |                    2 |           135 |
| 05342230050000 | TRUE        |      2018 |         2 |     2613 |                    2 |           135 |
| 05342230050000 | FALSE       |      2019 |         2 |     2613 |                   NA |            NA |
| 05342230050000 | FALSE       |      2020 |         2 |     2613 |                   NA |            NA |

## CCAO Colors

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
