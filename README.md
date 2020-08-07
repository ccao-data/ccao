
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CCAO package <a href='https://gitlab.com/ccao-data-science---modeling/packages/ccao'><img src='man/figures/logo.png' align="right" height="139" /></a>

A package to manage, distribute, and version control *CCAO-specific*
functions. These functions are used throughout CCAO applications,
models, and diagnostics. For generalized versions of assessment-related
functions, see
[assessR](https://gitlab.com/ccao-data-science---modeling/packages/assessr).

For detailed documentation on included functions and data, [**visit the
full reference
list**](https://ccao-data-science---modeling.gitlab.io/packages/ccao/reference/).

For examples of specific tasks you can complete with `ccao` functions,
see the [**vignettes
page**](https://ccao-data-science---modeling.gitlab.io/packages/ccao/articles/index.html).

## Installation

You can install the released version of `ccao` directly from GitLab by
running the following R command after installing
[remotes](https://github.com/r-lib/remotes):

``` r
remotes::install_gitlab("ccao-data-science---modeling/packages/ccao")
```

## Basic usage

Here is a quick example using `ccao` functions with included sample
data:

``` r
library(ccao)
library(dplyr)
library(knitr)

# Create a small subsample of data. This is the "raw" data taken from SQL
sample_data <- chars_sample_universe %>%
  select(PIN, TAX_YEAR, TOWN_CODE, AGE, GAR1_SIZE, BSMT) %>%
  slice(8:13) %>%
  distinct(TAX_YEAR, .keep_all = TRUE)

sample_data %>%
  kable(digits = 3)
```

| PIN            | TAX\_YEAR | TOWN\_CODE | AGE | GAR1\_SIZE | BSMT |
| :------------- | --------: | :--------- | --: | ---------: | ---: |
| 02031020010000 |      2018 | 29         |   4 |          3 |    1 |
| 02031020010000 |      2019 | 29         |   7 |          3 |    1 |
| 05081040120000 |      2013 | 23         |  80 |          8 |    2 |
| 05081040120000 |      2014 | 23         |  80 |          8 |    2 |

``` r

# Recode/rename/clean data using town_ and vars_ functions from ccao 
sample_data %>%
  mutate(
    PIN = pin_format_pretty(PIN),
    `Township Name` = town_convert(TOWN_CODE),
    `Township Triad` = town_get_triad(TOWN_CODE, name = TRUE),
    `Next Reass. Year` = town_get_assmnt_year(TOWN_CODE, round_type = "ceiling"),
    AGE = chars_fix_age(AGE, TAX_YEAR, TOWN_CODE)
  ) %>%
  vars_recode(type = "long") %>%
  vars_rename(names_from = "sql", names_to = "pretty") %>%
  kable(digits = 3)
```

| Property Identification Number | Year | Township Code | Age | Garage 1 Size | Basement | Township Name | Township Triad | Next Reass. Year |
| :----------------------------- | ---: | :------------ | --: | :------------ | :------- | :------------ | :------------- | ---------------: |
| 02-03-102-001                  | 2018 | 29            |   6 | 2 cars        | Full     | Palatine      | North          |             2022 |
| 02-03-102-001                  | 2019 | 29            |   7 | 2 cars        | Full     | Palatine      | North          |             2022 |
| 05-08-104-012                  | 2013 | 23            |  80 | 4 cars        | Slab     | New Trier     | North          |             2022 |
| 05-08-104-012                  | 2014 | 23            |  81 | 4 cars        | Slab     | New Trier     | North          |             2022 |

## CCAO colors

The CCAO Communications Department created a palette of colors for CCAO
press materials and visualizations. Navy, gold, and buttermilk are the
colors used in the CCAO logo. Typically navy and gold are used for
discrete values in plots. The hex codes for these colors are available
via the named list `ccao_colors`.

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
