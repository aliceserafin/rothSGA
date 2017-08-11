#' Apply plate normalization
#'
#' Adds `plate_effect` and `size_plate_norm` columns to a screenmill dataset.
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param of A column to apply the normalization to (as a string).
#' @param prefix A prefix appended to `plate_effect` and `plate_norm` column names.
#'
#' @md
#' @export

normalize_plate_effect <- function(sm_data, of, prefix) {

  assertthat::assert_that(assertthat::is.string(of), assertthat::is.string(prefix))
  effect <- paste0(prefix, 'plate_effect')
  norm   <- paste0(prefix, 'plate_norm')

  sm_data %>%
    split(.$plate_id) %>%
    lapply(plate_effect_normalization, of, effect, norm) %>%
    bind_rows()
}

plate_effect_normalization <- function(x, of, effect, norm) {
  x[[effect]] <- mean(x[[of]][x$plate_control], trim = 0.2)
  x[[norm]]   <- x[[of]] / x[[effect]]
  return(x)
}
