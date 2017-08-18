#' Batch effect normalization. Relies on the plate controls of each normalised (query and drug effect removed) plates, and correct the bias associated with batches from the same date
#'
#' @param df A data.frame with plate_id, query_id, date, cisplatin, plate_control_mean
#'
#' @md
#' @export




normalize_batch_effect <- function(df) {

  df$date <- as.character(df$date)

  data <- df %>%
    select(plate_id, query_id, date, cisplatin, plate_control_mean) %>%
    distinct() %>%
    group_by(cisplatin, query_id) %>%
    mutate(.Y = plate_control_mean / mean(plate_control_mean)) %>%
    ungroup()

  batch_effect <-
    lm(.Y ~ 0 + date, data) %>%
    coef(model) %>%
    enframe() %>%
    rename(date = name, batch_effect = value) %>%
    mutate(date = gsub('^date', '', date))

  left_join(df, batch_effect, by = 'date')
}
