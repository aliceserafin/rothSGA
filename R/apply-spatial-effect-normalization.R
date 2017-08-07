#' Apply spatial effect normalization for all plates
#'
#' Adds `spatial_effect` and `size_spatial_norm` columns to a screenmill dataset.
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param death_thresh Death threshold for excluding slow growing strains from model fitting. Defaults to `0.25`
#' @param ... Additional parameters passed to [stats::loess]
#'
#' @details Uses [stats::loess] to fit a two degree polynomial surface of the form $colony ~ row + column$.
#'
#' @md
#' @export

apply_spatial_effect_normalization <- function(sm_data, to, death_thresh = 0.25, ...) {

  assertthat::assert_that(assertthat::is.number(death_thresh))

  sm_data$to_temporary <- sm_data[[to]]

  sm_data %>%
    group_by(plate_id) %>%
    mutate(
      size_plate_norm = to_temporary / mean(to_temporary[plate_control], trim = 0.2),
      spatial_effect  = spatial_effect_loess(colony_row, colony_col, size_plate_norm, plate_control, value_thresh = death_thresh, ...),
      size_spatial_norm = to_temporary / spatial_effect
    ) %>%
    select(-size_plate_norm, -to_temporary) %>%
    ungroup()
}


# ---- Spatial effect prediction using loess ----
#
# Fits a loess smooth on row and column variables to predict a value.
# Plate controls are used for fitting the model which is then used to predict
# all values for all row + column combinations (defaults to all values). A value
# threshold is included to exclude values below this threshold when fitting the
# model (defaults to 0.25).

spatial_effect_loess <- function(row, col, value, plate_control = TRUE, value_thresh, deg = 2, ...) {
  data <- data_frame(row, col, value, plate_control)
  model_data <- filter(data, plate_control, value > value_thresh)
  model <- loess(value ~ row + col, data = model_data, degree = deg, normalize = FALSE, ...)
  return(predict(model, data))
}
