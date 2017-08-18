#' Exclude large colonies
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill]).
#' @param thresh Threshold for identifying large colonies (recommend using `1.5`).
#'
#' @md
#' @export

exclude_large_colonies <- function(sm_data, thresh) {
  sm_data %>%
    group_by(plate_id) %>%
    mutate(
      .plate_mean = mean(size[plate_control], trim = 0.2),
      .plate_norm = size / .plate_mean,
      .exclude = .plate_norm > thresh
    ) %>%
    # Exclude any cross that had a single colony that was speeding
    group_by(query_id, bio_replicate, plate, row, column) %>%
    mutate(.exclude = any(.exclude)) %>%
    filter(!.exclude) %>%
    select(-starts_with('.'))
}
