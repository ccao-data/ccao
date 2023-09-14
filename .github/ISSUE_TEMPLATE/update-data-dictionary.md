---
name: Update data dictionary
about: Steps to take when updating a data dictionary
title: "Update [DATA_DICT]"
---

# Data dictionary update checklist

- [ ] Make your updates to the source data file and commit them in a branch.
- [ ] Run `data-raw/<data_dict>.R`. This will save the CSV as an R data file (`data/<data_dict>.Rda`). This file is what the unit tests and package run against.
- [ ] Update the docstring for the data dict in `R/data.R`. This is where help docs for the packaged data.frame are kept. It needs to be updated manually to reflect the changes to the raw data.
- [ ] Run `devtools::test()` to make sure all unit tests pass with the new dictionary.
- [ ] Run `devtools::document()` to generate new man files from the changes to `R/data.R`.
- [ ] Run `devtools::check()` to run a test package build.
- [ ] Run `pkgdown::build_site()` and navigate to the reference page to make sure the changes are reflected on the docs website.
- [ ] Commit changes made by the preceding commands, push them to GitHub, and request a review.
