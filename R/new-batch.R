#' Create a new batch report
#'
#' Writes an Rmarkdown report to the current working directory with a recommended pipeline for
#' analyzing images, normalizing measurements, and generating quality control tables and figures.
#'
#' @param proj_dir Path to project directory
#' @param data_dir Path to data directory
#' @param author Name of author
#'
#' @importFrom rlang %||%
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_replace_all fixed
#' @export

new_batch <- function(proj_dir = '.', data_dir = 'data', author = getOption('devtools.name')) {
  assertthat::assert_that(dir.exists(proj_dir), dir.exists(data_dir), assertthat::is.string(author %||% ''))
  new_batch_processing(proj_dir, data_dir, author)
  new_quality_control(proj_dir, data_dir, author)
}


new_batch_processing <- function(proj_dir, data_dir, author) {
  read_lines(system.file('templates/batch-processing.Rmd', package = 'rothSGA')) %>%
    str_replace_all(fixed('{{{ author }}}'),   author %||% '') %>%
    str_replace_all(fixed('{{{ data_dir }}}'), data_dir) %>%
    str_replace_all(fixed('{{{ date }}}'),     Sys.Date()) %>%
    write_lines(file.path(proj_dir, 'Batch-processing.Rmd'))
}

new_quality_control <- function(proj_dir, data_dir, author) {
  read_lines(system.file('templates/quality-control.Rmd', package = 'rothSGA')) %>%
    str_replace_all(fixed('{{{ author }}}'),   author %||% '') %>%
    str_replace_all(fixed('{{{ data_dir }}}'), data_dir) %>%
    str_replace_all(fixed('{{{ date }}}'),     Sys.Date()) %>%
    write_lines(file.path(proj_dir, 'Quality-control.Rmd'))
}
