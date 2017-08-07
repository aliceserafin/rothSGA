#' Add biological replicate annotations
#' 
#' Creates a file named `biological-replicate-annotation.csv` with the 
#' minimal columns necessary to easily annotated biological replicates
#' for every plate. Currently only supports single timepoint analysis.
#' 
#' @param dir Path to screenmill analysis directory
#' @param overwrite A flag indicating whether to overwrite an existing biological replicate
#' annotation file.
#' 
#' @import dplyr
#' @import assertthat
#' @importFrom readr write_csv
#' @md


add_biological_replicates <- function(dir, overwrite = FALSE) {
  
  anno_path <- file.path(dir, 'screenmill-annotations.csv')
  quer_path <- file.path(dir, 'screenmill-queries.csv')
  
  assertthat::assert_that(
    dir.exists(dir),
    file.exists(anno_path),
    file.exists(quer_path),
    assertthat::is.flag(overwrite)
  )
  
  goal <- file.path(dir, 'biological-replicate-annotation.csv')
  
  # Biological replicates need to be manually entered into the following table
  if (!file.exists(goal) || overwrite) {
    anno    <- read_csv(anno_path)
    queries <- read_csv(quer_path)
    left_join(anno, queries) %>%
      mutate(bio_replicate = '') %>%
      select(plate_id, query_name, query_id, group, position) %>%
      write_csv(goal)
    message('Please manually annotate biological replicates in:\n  ', goal)
  } else {
    message('Biological replicates have already been added. Please manually edit the following file, or set overwrite = TRUE:\n  ', goal)
  }
  return(invisible())
}
