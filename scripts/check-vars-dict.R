# Script to check that the `vars_dict` data object is well-formed
load("data/vars_dict.rda")

# Check for duplicate model parameters
non_na_model_vars <- subset(
  vars_dict,
  !is.na(var_name_model)
)[c("var_name_model", "var_code", "var_value")]
dupes <- non_na_model_vars[which(duplicated(non_na_model_vars)), ]

if (nrow(dupes) > 0) {
  stop(
    "Duplicate var_name_model entries in vars_dict: ",
    paste(dupes$var_name_model, collapse = ", ")
  )
}
