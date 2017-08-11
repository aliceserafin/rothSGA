#' Add biological replicate annotations
#'
#' Creates a file named `biological-replicate-annotation.csv` with the
#' minimal columns necessary to easily annotated biological replicates
#' for every plate. Currently only supports single timepoint analysis.
#'
#' @param dir Path to screenmill analysis directory
#' @param file Name of resulting CSV file.
#' @param overwrite A flag indicating whether to overwrite an existing biological replicate
#' annotation file.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom readr write_csv read_csv
#' @md
#' @export

add_biological_replicates <- function(dir, file, overwrite = FALSE) {
  anno_path <- file.path(dir, 'screenmill-annotations.csv')
  quer_path <- file.path(dir, 'screenmill-queries.csv')

  assertthat::assert_that(
    dir.exists(dir),
    file.exists(anno_path),
    file.exists(quer_path),
    assertthat::is.flag(overwrite),
    assertthat::is.string(file)
  )

  goal <- file.path(dir, 'biological-replicate-annotation.csv')

  # Biological replicates need to be manually entered into the following table
  if (!file.exists(goal) || overwrite) {
    anno    <- readr::read_csv(anno_path, col_types = cols_only(query_id = 'c', plate_id = 'c', group = 'i', position = 'i'))
    queries <- readr::read_csv(quer_path, col_types = cols_only(query_id = 'c', query_name = 'c'))
    left_join(anno, queries) %>%
      mutate(bio_replicate = '') %>%
      select(plate_id, query_name, query_id, group, position, bio_replicate) %>%
      write_csv(goal)
    message('Please manually annotate biological replicates in:\n  ', goal)
  } else {
    message('Biological replicates have already been added. Please manually edit the following file, or set overwrite = TRUE:\n  ', goal)
  }
  return(invisible())
}
