#' Aggregates and summarizes technical replicates on the plate
#'
#' Adds mean_size_norm , median_size_norm, and sd_size_norm to a screenmill dataset.
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param to A column to apply the stats. default is size_plate_norm (see[rothSGA::apply_plate_normalization]) (as a string).
#'
#' @md
#' @export


summarize_normalized_data <- function(sm_data, to = 'size_plate_norm'){

  assertthat::assert_that(assertthat::is.string(to))

  sm_data$to_tmp <- sm_data[[to]]

  sm_data %>%
    group_by(plate_id, strain_name) %>%
    mutate(
      n                = n(),
      mean_size_norm   = mean(to_tmp),
      median_size_norm = median(to_tmp),
      sd_size_norm     = sd(to_tmp)
    ) %>%
    ungroup()

}
