
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

Occasionally, when using brand-new or source versions of packages,
installation [on Windows will fail with the following
error](https://github.com/rstudio/renv/issues/162):

    DLL 'package_name' not found: maybe not installed for this architecture?

If this happens, try using the following installation command:

``` r
remotes::install_gitlab(
  repo = "ccao-data-science---modeling/packages/ccao",
  INSTALL_opts = "--no-multiarch"
)
```

## Basic usage

Here is a quick example using `ccao` functions with included sample
data:

``` r
library(ccao)
library(dplyr)
library(knitr)

# Create a small subsample of data. This is the "raw" data taken from SQL
sample_data <- chars_sample_athena %>%
  select(pin, year, char_yrblt, char_gar1_size, char_ext_wall) %>%
  slice(c(1, 2, 5, 14)) %>%
  mutate(township_code = c("72", "73", "71", "72"))

sample_data %>%
  kable(digits = 3)
```

| pin            | year | char_yrblt | char_gar1_size | char_ext_wall | township_code |
|:---------------|:-----|-----------:|:---------------|:--------------|:--------------|
| 19233270420000 | 2020 |       1950 | 7              | 3             | 72            |
| 14072030040000 | 2020 |       1908 | 3              | 1             | 73            |
| 13324160010000 | 2020 |       1920 | 2              | 2             | 71            |
| 19252040400000 | 2020 |       1916 | 1              | 4             | 72            |

``` r
# Recode/rename/clean data using town_ and vars_ functions from ccao 
sample_data %>%
  mutate(
    pin = pin_format_pretty(pin),
    township_name = town_convert(township_code),
    triad_name = town_get_triad(township_code, name = TRUE),
    `Next Reass. Year` = town_get_assmnt_year(
      township_code,
      round_type = "ceiling"
    )
  ) %>%
  vars_recode(type = "long") %>%
  vars_rename(names_from = "athena", names_to = "pretty") %>%
  kable(digits = 3)
```

| PIN           | Year | Year Built | Garage 1 Size | Exterior Wall Material | Township Code | Township Name | Triad Name | Next Reass. Year |
|:--------------|:-----|-----------:|:--------------|:-----------------------|:--------------|:--------------|:-----------|-----------------:|
| 19-23-327-042 | 2020 |       1950 | 0 cars        | Frame + Masonry        | 72            | Lake          | City       |             2024 |
| 14-07-203-004 | 2020 |       1908 | 2 cars        | Frame                  | 73            | Lake View     | City       |             2024 |
| 13-32-416-001 | 2020 |       1920 | 1.5 cars      | Masonry                | 71            | Jefferson     | City       |             2024 |
| 19-25-204-040 | 2020 |       1916 | 1 cars        | Stucco                 | 72            | Lake          | City       |             2024 |

## CCAO colors

The CCAO Communications Department created a palette of colors for CCAO
press materials and visualizations. Navy, gold, and buttermilk are the
colors used in the CCAO logo. Typically navy and gold are used for
discrete values in plots. The hex codes for these colors are available
via the named list `ccao_colors`.

    #> Warning: It is deprecated to specify `guide = FALSE` to remove a guide. Please
    #> use `guide = "none"` instead.

<img src="man/figures/README-colors-1.png" width="100%" />
