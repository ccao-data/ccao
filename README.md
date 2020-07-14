
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

  - `chars_288_active()` returns a vector of active years given a 288
    start date and township
  - `chars_fix_age()` fixes CCAO property ages that only update every
    assessment cycle
  - `chars_sparsify()` converts ADDCHARS SQL data to a sparse format
    that can be joined to normal chars datasets
  - `chars_update()` updates the specified characteristic columns using
    each column’s ADDCHARS data

-----

  - `check_class()` checks if a property class falls within its expected
    square footage and age boundaries

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
  - `chars_cols` is a list crosswalk of CCAOSFCHARS columns and their
    ADDCHARS equivalents
  - `chars_dict` is a crosswalk of human-readable translations of CCAO
    database characteristic codes
  - `chars_sample_addchars` is a sample of data extracted from the
    ADDCHARS SQL table
  - `chars_sample_universe` is a sample of data extracted from the
    VW\_RES\_UNIVERSE view. It contains the same PINS as
    `chars_sample_addchars`.
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

## Handling for 288s (Home Improvement Exemptions)

The State of Illinois has a home improvement exemption program which
allows property owners to deduct up to $75,000 per year of any value
created by improvements to a residential property.

This has the effect of essentially “freezing” a home’s characteristics
at whatever they were prior to the start of the improvement project. For
example, if a property owner adds an additional bedroom and applies for
a 288, the property will be valued as if the new bedroom does not exist
until the 288 expires and as long as the increase in valuation is less
than $75,000.

[Per Illinois
statute](https://www.ilga.gov/legislation/ilcs/fulltext.asp?DocName=003502000K15-180),
288s expire after 4 years or until the next assessment cycle, whichever
is longer. For example, a 288 received in 2016 for a property in
Northfield (with assessment years 2016, 2019, and 2022) will last 6
years (until 2021, the year before the 2022 reassessment).

The `chars_` set of functions are designed to make handling 288s simpler
and more consistent. Two sample datasets, `chars_sample_addchars` and
`chars_sample_universe` are included to simulate real-world use. The
`chars_sample_addchars` dataset is a direct sample of data from the
ADDCHARS SQL table and includes individual rows listing the PIN, start
date, class, and characteristic updates associated with a 288 Home
Improvement Exemption. This data format is difficult to work with and
complicated by that fact that multiple 288s can be active at the same
time for different periods, and some columns from ADDCHARS add to
existing characteristics, while some overwrite existing characteristics.

The included `chars_sparsify()` function transforms these single rows
into a sparse data frame which lists a row and characteristic update per
PIN per year per class. This is most easily visualized with a mock
dataset:

The base ADDCHARS data syntax looks like:

| QU\_PIN | TAX\_YEAR | QU\_TOWN | QU\_UPLOAD\_DATE | QU\_SQFT\_BLD | QU\_ROOMS |
| ------- | --------- | -------- | ---------------- | ------------- | --------- |
| 12345   | 2013      | 77       | 130702           | 200           | 0         |
| 12345   | 2015      | 77       | 150703           | 300           | 1         |

This function will transform it into:

| QU\_PIN | YEAR | QU\_SQFT\_BLD | QU\_ROOMS |
| ------- | ---- | ------------- | --------- |
| 12345   | 2013 | 200           | 0         |
| 12345   | 2014 | 200           | 0         |
| 12345   | 2015 | 500           | 1         |
| 12345   | 2016 | 500           | 1         |
| 12345   | 2017 | 500           | 1         |
| 12345   | 2018 | 300           | 1         |
| 12345   | 2019 | 300           | 1         |
| 12345   | 2020 | 300           | 1         |

### Chars Function Usage

Below is an example showing the entire process of using the `chars_`
functions to update characteristics data using an ADDCHARS extract:

``` r
library(dplyr)
library(tidyr)
library(knitr)
library(ccao)

# Choose a random sample PIN from the sample ADDCHARS data
sample_chars <- chars_sample_addchars %>%
  filter(QU_PIN == "05273000030000")

# This PIN has a basement and garage renovation followed by a bathroom reno
# three years later
sample_chars %>%
  select(
    QU_PIN, TAX_YEAR, QU_CLASS,
    QU_TOWN, QU_GARAGE_SIZE, QU_SQFT_BLD, QU_BEDS
  ) %>%
  kable(format = "markdown", digits = 3)
```

| QU\_PIN        | TAX\_YEAR | QU\_CLASS | QU\_TOWN | QU\_GARAGE\_SIZE | QU\_SQFT\_BLD | QU\_BEDS |
| :------------- | --------: | --------: | -------: | ---------------: | ------------: | -------: |
| 05273000030000 |      2015 |     20600 |       23 |                3 |             0 |        0 |
| 05273000030000 |      2018 |     20600 |       23 |                0 |           384 |        1 |

``` r

# Sparsify the data. You can see that one 288 ends roughly when the other one
# begins. NOTE: the mutate() on QU_CLASS here is because sometime QU_CLASS is
# equal to 0 (mostly for garage renovations). This is an easy fix
sparse_chars <- sample_chars %>%
  arrange(QU_PIN, TAX_YEAR, QU_CLASS) %>%
  mutate(QU_CLASS = ifelse(QU_CLASS == 0, lag(QU_CLASS), QU_CLASS)) %>%
  chars_sparsify(
    pin_col = QU_PIN,
    year_col = TAX_YEAR, 
    class_col = QU_CLASS,
    town_col = as.character(QU_TOWN),
    upload_date_col = QU_UPLOAD_DATE,
    additive_source = any_of(chars_cols$add_source),
    replacement_source = any_of(chars_cols$rep_source)
  ) %>%
  mutate(QU_CLASS = substr(QU_CLASS, 1, 3))

# The resulting sparse data can be joined onto any data containing CCAOSFCHARS
sparse_chars %>%
  select(
    QU_PIN, YEAR, QU_CLASS, QU_GARAGE_SIZE, 
    QU_SQFT_BLD, QU_BEDS, NUM_288S_ACTIVE
  ) %>%
  kable(format = "markdown", digits = 3)
```

| QU\_PIN        | YEAR | QU\_CLASS | QU\_GARAGE\_SIZE | QU\_SQFT\_BLD | QU\_BEDS | NUM\_288S\_ACTIVE |
| :------------- | ---: | :-------- | ---------------: | ------------: | -------: | ----------------: |
| 05273000030000 | 2015 | 206       |                3 |             0 |        0 |                 1 |
| 05273000030000 | 2016 | 206       |                3 |             0 |        0 |                 1 |
| 05273000030000 | 2017 | 206       |                3 |             0 |        0 |                 1 |
| 05273000030000 | 2018 | 206       |                3 |           384 |        1 |                 2 |
| 05273000030000 | 2019 | 206       |                0 |           384 |        1 |                 1 |
| 05273000030000 | 2020 | 206       |                0 |           384 |        1 |                 1 |
| 05273000030000 | 2021 | 206       |                0 |           384 |        1 |                 1 |

``` r

# Here is an example data frame where the sparse data is merged onto
# characteristic data and the characteristics are then updated using chars_update()
updated_chars <- chars_sample_universe %>%
  filter(PIN %in% sample_chars$QU_PIN) %>%
  left_join(
    sparse_chars,
    by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR", "CLASS" = "QU_CLASS")
  ) %>%
  arrange(PIN, TAX_YEAR) %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add_target),
    replacement_target = any_of(ccao::chars_cols$rep_target)
  )

# Show updated characteristics vs ADDCHARS
updated_chars %>%
  select(
    PIN, TAX_YEAR, CLASS, NUM_288S_ACTIVE, GAR1_SIZE, QU_GARAGE_SIZE,
    BLDG_SF, QU_SQFT_BLD, BEDS, QU_BEDS
  ) %>%
  arrange(PIN, CLASS, TAX_YEAR) %>%
  kable(format = "markdown", digits = 3)
```

| PIN            | TAX\_YEAR | CLASS | NUM\_288S\_ACTIVE | GAR1\_SIZE | QU\_GARAGE\_SIZE | BLDG\_SF | QU\_SQFT\_BLD | BEDS | QU\_BEDS |
| :------------- | --------: | :---- | ----------------: | ---------: | ---------------: | -------: | ------------: | ---: | -------: |
| 05273000030000 |      2015 | 202   |                NA |          1 |               NA |      928 |            NA |    2 |       NA |
| 05273000030000 |      2016 | 202   |                NA |          1 |               NA |      928 |            NA |    2 |       NA |
| 05273000030000 |      2017 | 202   |                NA |         NA |               NA |        0 |            NA |    0 |       NA |
| 05273000030000 |      2018 | 202   |                NA |          1 |               NA |      928 |            NA |    2 |       NA |
| 05273000030000 |      2019 | 202   |                NA |          1 |               NA |      928 |            NA |    2 |       NA |
| 05273000030000 |      2020 | 202   |                NA |          1 |               NA |      928 |            NA |    2 |       NA |
| 05273000030000 |      2013 | 206   |                NA |          1 |               NA |     2637 |            NA |    5 |       NA |
| 05273000030000 |      2014 | 206   |                NA |          1 |               NA |     2637 |            NA |    5 |       NA |
| 05273000030000 |      2015 | 206   |                 1 |          3 |                3 |     2637 |             0 |    5 |        0 |
| 05273000030000 |      2016 | 206   |                 1 |          3 |                3 |     2637 |             0 |    5 |        0 |
| 05273000030000 |      2017 | 206   |                 1 |          3 |                3 |      928 |             0 |    2 |        0 |
| 05273000030000 |      2018 | 206   |                 2 |          3 |                3 |     3916 |           384 |    6 |        1 |
| 05273000030000 |      2019 | 206   |                 1 |          7 |                0 |     3916 |           384 |    6 |        1 |
| 05273000030000 |      2020 | 206   |                 1 |          7 |                0 |     3916 |           384 |    6 |        1 |

## Common Spatial Boundaries

This package contains spatial data frames representing CCAO
administrative boundaries. Note that you **must have the `sf` package
installed and loaded** in order to correctly load spatial data frames.

``` r
library(sf)

# Plot township boundaries alone
plot(ccao::town_shp[1], main = "Township Boundaries")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
# Plot township boundaries with neighborhoods
plot(ccao::nbhd_shp[1], main = "Townships with Neighborhoods")
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

## CCAO Colors

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
