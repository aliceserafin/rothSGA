#' Creates 'WARNING_mixed_array.csv' as an easy to read automated report of the problematic plates seemingly mixed or wrong or contaminated.
#' vsp33 is absent from the array and should always be a blank spot.
#'
#' @param dir A directory containing the screenmill data
#' @param death_tresh Death threshold for excluding potential pinning residues. default set to '10', stringent treshold.
#'
#'
#' @md
#' @export


check_dead_strains <- function(dir, death_tresh) {

  assertthat::assert_that(assertthat::is.writeable(dir))
  assertthat::assert_that(assertthat::is.number(death_thresh))

  bio_replicate_file <- read_csv('biological-replicate-annotation.csv')


  sm_data <- screenmill::read_screenmill(dir) %>%
    left_join(bio_replicate_file) %>%
    mutate(
      # Make a numeric cisplatin variable by extracting the number between "-" and "uM"
      cisplatin = as.numeric(str_extract(treatment_id, '(?<=-).*(?=(uM))'))
    )

  sm_data %>%
    group_by(query_name, bio_replicate, strain_name) %>%
    summarise(
      mean_growth = mean(size[cisplatin == 0 & strain_name == 'vps33'])
    ) %>%
    filter(mean_growth > 10 ) %>%
    write_csv('WARNING_mixed_array.csv')

}
