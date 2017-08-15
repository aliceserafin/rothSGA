#' Check for growth of the self-cross. Query strains and array strains K.O. at the same locus should not grow,
#' because geneX::KanMX and geneX::NatMX cannot be both present in an haploid double mutant.
#'
#'
#'
#' @param dir A directory containing the screenmill data
#' @param untreated The name of the untreated condition under the column treatment_id, default is 'CisPt-0uM' (see [screenmill::read_screenmill]) (as a string)
#'
#' @details This will only be applied to the untreated plates.
#'
#' @md
#' @export

check_self_crosses <- function(dir, untreated = 'CisPt-0uM'){

  assertthat::assert_that(assertthat::is.writeable(dir))
  assertthat::assert_that(assertthat::is.string(untreated))

  bio_replicate_file <- read_csv(file.path(dir, 'biological-replicate-annotation.csv'))


  sm_data <- screenmill::read_screenmill(dir) %>%
    left_join(bio_replicate_file) %>%
    mutate(
      # Make a numeric cisplatin variable by extracting the number between "-" and "uM"
      cisplatin = as.numeric(str_extract(treatment_id, '(?<=-).*(?=(uM))'))
    )

  results <- sm_data %>%
    filter(treatment_id == untreated) %>%
    group_by(query_name, bio_replicate) %>%
    summarise(mean_growth_self_cross = mean(size[strain_name == query_name])) %>%
    filter(mean_growth_self_cross > 25) #Self-cross definition

  print(results)

}

