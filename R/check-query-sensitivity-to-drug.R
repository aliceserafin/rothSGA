#' Check if the overall growth on the plates is too low, indicative of a too high concentration of the drug for
#' this query biological replicate
#'
#' Creates 'WARNING_too_sick.csv' as an easy to read automated report of the problematic plates.
#' Creates 'Query_sensitivity.csv' as an easy to read automated report of the effect of the highest drug treatment.
#' Sweet spot for the differential epistasis map is defined as a drug+mutant specific 30% fitness defect, with a 40-50% maximum
#' fitness defect compared to the untreated WT strain.
#'
#'
#' @param dir A directory containing the screenmill data
#'
#' @param spatial_normalized Logical. Should the sensitivity check be done on spatially normalized data ? default is TRUE (see[rothSGA::apply_spatial_effect_normalization])
#'
#' @md
#' @export



check_query_sensitivity_to_drug <- function(dir, normalized = TRUE) {

  assertthat::assert_that(assertthat::is.writeable(dir))
  assertthat::assert_that(assertthat::is.flag(normalized))

  if (normalized == TRUE) {
    sm_data <- read_csv(file.path(dir, 'processed-colonies.csv'))
    value = 'size_spatial_norm'
  } else {
    sm_data <- screenmill::read_screenmill(dir)
    value = 'size'
  }

  bio_replicate_file <- read_csv(file.path(dir, 'biological-replicate-annotation.csv'))


  #sm_data <- screenmill::read_screenmill(dir) %>%
  #  filter(
  #    !is.na(strain_name),
  #    !is.na(size)
  #  ) %>%
  #  left_join(bio_replicate_file) %>%
  #  mutate(
  #    # Make a numeric cisplatin variable by extracting the number between "-" and "uM"
  #    cisplatin = as.numeric(str_extract(treatment_id, '(?<=-).*(?=(uM))')),
  #    WT = mean(value[cisplatin == 0 & strain_name == 'his3' & query_name == 'leu2'], trim = 0.2)
  #  )

  results <-
    read_csv('data/processed-colonies.csv') %>%
    filter(
      !is.na(strain_name),
      !is.na(size)
    ) %>% # Enlever les valeurs NA qui font tout beuger
    left_join(bio_replicate_file) %>%
    mutate(
      # Make a numeric cisplatin variable by extracting the number between "-" and "uM"
      cisplatin = as.numeric(str_extract(treatment_id, '(?<=-).*(?=(uM))')),
      # "WT" definition - Mean of center 60% of WT [0uM his3 leu2]
      WT = mean(size[cisplatin == 0 & strain_name == 'his3' & query_name == 'leu2'], trim = 0.2)
    )

  results %>%
    group_by(query_name, bio_replicate) %>% # For every query+replicate
    summarise(
      # Mean of center 60% of query single mutants [<max>uM his3]
      query_highest_treatment_mean = mean(results$size_spatial_norm[results$cisplatin == max(results$cisplatin) & results$strain_name == 'his3'], trim = 0.2) / mean(results$WT)) %>%
    filter(query_highest_treatment_mean < 0.6) %>% # Too-sick definition - below 60% of WT
    write_csv('data/WARNING_too_sick.csv')


  his3 <-
    results %>%
    filter(strain_name == 'his3') %>%
    select(plate_id, strain_name, query_name, cisplatin, bio_replicate, value)

  WT_no_drug <-
    his3 %>%
    filter(query_name == 'leu2' & cisplatin == 0) %>%
    summarise(WT_no_drug = mean(size_spatial_norm, trim = 0.2)) %>%
    pull(WT_no_drug)

  WT_drug <-
    his3 %>%
    filter(query_name == 'leu2' & cisplatin != 0) %>%
    group_by(cisplatin) %>%
    summarise(WT_drug = mean(size_spatial_norm, trim = 0.2))

  Query_no_drug <-
    his3 %>%
    filter(query_name != 'leu2' & cisplatin == 0) %>%
    group_by(query_name, bio_replicate) %>%
    summarise(Query_no_drug = mean(size_spatial_norm, trim = 0.2))

  Query_drug <-
    his3 %>%
    filter(query_name != 'leu2' & cisplatin != 0) %>%
    group_by(query_name, cisplatin, bio_replicate) %>%
    summarise(Query_drug = mean(size_spatial_norm, trim = 0.2))

  Drug_interaction <-
    Query_drug %>%
    left_join(Query_no_drug, by = c('query_name', 'bio_replicate')) %>%
    left_join(WT_drug, by = c('cisplatin')) %>%
    mutate(
      WT_no_drug    = WT_no_drug,
      ratio_drug    = Query_drug / WT_drug,
      ratio_no_drug = Query_no_drug / WT_no_drug,
      difference    = ratio_drug - ratio_no_drug
    ) %>%
    group_by(query_name) %>%
    mutate(max_drug = cisplatin == max(cisplatin))

  write_csv(Drug_interaction, 'Query_sensitivity.csv')

  cat('query_sensitivity.csv file created in data directory !')


}

