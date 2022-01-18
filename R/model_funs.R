#' Strip attribute data from CV objects to shrink their size
#'
#' @description Remove unnecessary attribute data from iteration results objects
#' created by \code{\link[tune]{tune_grid}} and \code{\link[tune]{tune_bayes}}.
#' This makes the parameter object significantly smaller when saved to disk.
#'
#' @param x Iteration object created by the tune package.
#'
#' @return Iteration object from \code{x} but with CV split and attributes
#'   removed.
#'
#' @family model_funs
#' @export
model_axe_tune_data <- function(x) {
  stripped <- dplyr::select(x, -dplyr::any_of("splits"))
  attrs <- purrr::list_modify(attributes(x), "names" = names(stripped))
  attributes(stripped) <- attrs

  return(stripped)
}


#' Strip environment data from recipe objects to shrink their size
#'
#' @description Recipe objects used by Tidymodels sometimes become bloated due
#' to quosures and saved factor levels. This function will strip all data from
#' a recipe object except the data necessary to use \code{\link[recipes]{bake}}.
#'
#' This saves an enormous amount of disk space for certain complicated recipes
#' that need to be saved and re-used.
#'
#' @param x A prepped recipe object created by the recipe package.
#'
#' @return Recipe object from \code{x} but with environment and factor level
#'   data removed.
#'
#' @family model_funs
#' @export
model_axe_recipe <- function(x) {
  stopifnot(class(x) == "recipe")

  axed <- rapply(x, butcher::butcher, how = "replace")
  axed <- purrr::list_modify(axed, orig_lvls = NULL)
  class(axed) <- "recipe"

  return(axed)
}


#' Cap the num_leaves hyperparameter for LightGBM models
#'
#' @description LightGBM grows trees leaf-wise rather than depth-wise, meaning
#' that not all tree levels will have the same number of leaves.
#'
#' \code{num_leaves} is implicitly capped at \code{2 ^ max_depth} (full depth
#' tree) in LightGBM, but saved parameter objects can still show higher numbers.
#'
#' This function explicitly caps \code{num_leaves} in saved parameter
#' objects by setting \code{num_leaves} equal to \code{2 ^ max_depth - 1}
#' where appropriate. Note that in parsnip, \code{max_depth} is substituted by
#' the \code{tree_depth} hyperparameter.
#'
#' @param params A dataframe of parameters containing both \code{num_leaves} and
#'   \code{tree_depth}.
#'
#' @examples
#' params <- data.frame(num_leaves = c(60, 600), tree_depth = c(7, 7))
#' model_lgbm_cap_num_leaves(params)
#' @return A dataframe of parameters with \code{num_leaves} capped at
#'   \code{2 ^ max_depth - 1}.
#'
#' @family model_funs
#' @export
model_lgbm_cap_num_leaves <- function(params) {
  params$num_leaves <- ifelse(
    params$num_leaves > (2^params$tree_depth) - 1,
    max((2^params$tree_depth) - 1, 2),
    params$num_leaves
  )
  return(params)
}


#' Finalize workflow objects for LightGBM models
#'
#' @description LightGBM is currently not natively supported by Tidymodels.
#' As such, functions such as \code{\link[tune]{finalize_workflow}}, which is
#' usually responsible for passing final hyperparameters from cross-validation
#' into a workflow object, do not function correctly.
#'
#' This function is a hacky workaround which inserts parameters directly into
#' the workflow object by looking for their names (workflows are just nested
#' lists), then inserting the matching values passed in \code{params}.
#'
#' This function will likely be deprecated by in the near future.
#'
#' @param wflow A workflow object created by \code{\link[workflows]{workflow}}.
#'
#' @param params A dataframe or named list of parameters. Usually created by
#'   \code{\link[tune]{select_best}}.
#'
#' @return A finalized workflow object with updated parameters.
#'
#' @family model_funs
#' @export
model_lgbm_update_params <- function(wflow, params) {
  wflow$fit$actions$model$spec <- purrr::list_modify(
    wflow$fit$actions$model$spec,
    as.list(dplyr::select(params, -dplyr::any_of(".config")))
  )
  return(wflow)
}


#' Save a LightGBM model to disk
#'
#' Save a parsnip model fit object with a LightGBM fit to disk. This is a
#' a workaround to split the LightGBM model and parsnip specification into
#' separate files and then zip them together. This is necessary because LightGBM
#' models really do not like being saved with \code{saveRDS()}.
#'
#' @param model A parsnip specification containing a fit LightGBM model.
#' @param zipfile A path to save zip file to.
#'
#' @family model_funs
#' @export
model_lgbm_save <- function(model, zipfile) {
  file_lgbm <- file.path(tempdir(), "lgbm.model")
  lightgbm::lgb.save(model$fit, file_lgbm)
  model$fit <- NULL

  file_meta <- file.path(tempdir(), "meta.model")
  saveRDS(model, file_meta)

  zip::zipr(zipfile, files = c(file_meta, file_lgbm))
}


#' Load a LightGBM model from disk
#'
#' This is the paired function to \code{model_lgbm_save()}. It loads a zip file
#' containing a parsnip specification and LightGBM model, then recombines them
#' into a single object.
#'
#' @param zipfile A path to the zip file containing the saved model objects.
#'
#' @return A parsnip model fit with LightGBM model.
#'
#' @family model_funs
#' @export
model_lgbm_load <- function(zipfile) {
  ex_dir <- tempdir()
  zip::unzip(zipfile, exdir = ex_dir)

  model <- readRDS(file.path(ex_dir, "meta.model"))
  model$fit <- lightgbm::lgb.load(file.path(ex_dir, "lgbm.model"))

  return(model)
}
