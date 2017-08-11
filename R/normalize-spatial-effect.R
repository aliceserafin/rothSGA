#' Apply spatial effect normalization for all plates
#'
#' Adds `spatial_effect` and `size_spatial_norm` columns to a screenmill dataset.
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param of A column to apply the normalization to (as a string).
#' @param death_thresh Death threshold for excluding slow growing strains from model fitting.
#' @param prefix A prefix appended to `plate_effect` and `plate_norm` column names.
#' @param ... Additional parameters passed to [stats::loess]
#'
#' @details Uses [stats::loess] to fit a two degree polynomial surface of the form $colony ~ row + column$.
#'
#' @md
#' @export

normalize_spatial_effect <- function(sm_data, of, death_thresh, prefix, ...) {

  assertthat::assert_that(
    assertthat::is.string(of),
    assertthat::is.string(prefix),
    assertthat::is.number(death_thresh)
  )

  effect <- paste0(prefix, 'spatial_effect')
  norm   <- paste0(prefix, 'spatial_norm')

  sm_data %>%
    split(.$plate_id) %>%
    lapply(spatial_effect_normalization, of, effect, norm, death_thresh, ...) %>%
    bind_rows()
}

spatial_effect_normalization <- function(x, of, effect, norm, death_thresh, ...) {
  .plate_normalized <- x[[of]] / mean(x[[of]][x$plate_control], trim = 0.2)
  x[[effect]] <- with(x, spatial_effect_loess(colony_row, colony_col, .plate_normalized, plate_control, death_thresh, ...))
  x[[norm]]   <- x[[of]] / x[[effect]]
  return(x)
}

# ---- Spatial effect prediction using loess ----
#
# Fits a loess smooth on row and column variables to predict a value.
# Plate controls are used for fitting the model which is then used to predict
# all values for all row + column combinations (defaults to all values). A value
# threshold is included to exclude values below this threshold when fitting the
# model (defaults to 0.25).

spatial_effect_loess <- function(row, col, value, plate_control, value_thresh, deg = 2, ...) {
  data <- data_frame(row, col, value, plate_control)
  model_data <- filter(data, plate_control, value > value_thresh)
  model <- loess(value ~ row + col, data = model_data, degree = deg, normalize = FALSE, ...)
  return(predict(model, data))
}
