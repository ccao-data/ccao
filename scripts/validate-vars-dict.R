load("data/vars_dict.rda")

non_na_model_vars <- subset(
  vars_dict,
  !is.na(var_name_model)
)[c("var_name_model", "var_code", "var_value")]

dupes <- non_na_model_vars[which(duplicated(non_na_model_vars)), ]

if (nrow(dupes) > 0) {
  dupe_var_names <- dupes$var_name_model
  stop(
    "Duplicate var_name_model entries in vars_dict: ",
    paste(dupe_var_names, collapse = ", ")
  )
}
