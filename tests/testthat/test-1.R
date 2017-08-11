library(rothSGA)

re_annotate  =  FALSE
re_calibrate =  FALSE
re_measure   =  FALSE
needs_review =  FALSE
dir          = 'screenmill-data'

add_biological_replicates(dir, file = 'biological-replicate-annotation.csv')

if (needs_review) screenmill::review(dir)

sm_data <-
  dplyr::left_join(
    screenmill::read_screenmill(dir),
    readr::read_csv(file.path(dir, 'biological-replicate-annotation.csv'))
  )

sm_no_large <- exclude_large_colonies(sm_data, thresh = 1.5)
spatial_normalized <- normalize_spatial_effect(sm_no_large, of = 'size', death_thresh = 0.25, prefix = 'size_')
plate_normalized <- normalize_plate_effect(spatial_normalized, of = 'size_spatial_norm', prefix = 'size_spatial_')

nrow(sm_data) - nrow(sm_no_large)
test <- purrr::map(seq(1, 2, by = 0.1), function(thresh) {
  .no_large <- exclude_large_colonies(sm_data, thresh = thresh)
  ((nrow(sm_data) - nrow(.no_large)) / 4) / length(unique(sm_data$plate_id))
})

plot(x = seq(1, 2, by = 0.1), y = test)
abline(h = 10, lty = 'dotted')
abline(h = 5, lty = 'dotted')
abline(v = 1.5, lty = 'dotted')
