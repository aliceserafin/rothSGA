#' Apply plate normalization
#'
#' Adds `plate_effect` and `size_plate_norm` columns to a screenmill dataset.
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param to A column to apply the normalization to (as a string).
#'
#' @md
#' @export

apply_plate_normalization <- function(sm_data, to = 'size') {

  assertthat::assert_that(assertthat::is.string(to))

  sm_data$to_temporary <- sm_data[[to]]

  sm_data %>%
    group_by(plate_id) %>%
    mutate(
      plate_effect    = mean(to_temporary[plate_control], trim = 0.2),
      size_plate_norm = to_temporary / plate_effect
    ) %>%
    select(-to_temporary) %>%
    ungroup()
}
