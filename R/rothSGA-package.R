#' SGA analysis tools for the Rothstein Lab.
#'
#' Tools in this package are provided to standardize common steps in the
#' quantification of genetic interactions derived from Synthetic Genetic Array
#' ([SGA](https://en.wikipedia.org/wiki/Synthetic_genetic_array)) colony growth
#' experiments.
#'
#' @details Groups of SGA experiments performed in parallel at the same time
#' are considered part of the same "Batch". We recommend processing each
#' batch independently using approximately the following workflow:
#'
#' 1. Record colony growth (ideal incubation time will depend on several factors
#' including: strain background, media, temperature, pinning procedure). We
#' recommend using a flatbed scanner with a transparency lid (e.g. the
#' [Micotek 9800XL](https://www.amazon.com/Microtek-ScanMaker-Scanner-Transparent-Adapter/dp/B000068CNE)
#' can scan 9 standard rectangular plates.)
#'
#' 2. Annotate plates in images with [screenmill::annotate] which integrates
#' with the Rothstein Lab's annotation database [rothfreezer::rothfreezer]
#'
#' 3. Locate colony grid and determine plate cropping coordinates with
#' [screenmill::calibrate]
#'
#' 4. Review plates to exclude regions with contamination or failed pinnings
#' with [screenmill::review]
#'
#' 5. Measure colonies with [screenmill::measure]
#'
#' 6. Normalize spatial and plate effects with
#' [rothSGA::normalize_spatial_effect] and [rothSGA::normalize_plate_effect]
#'
#' 7. Run quality control checks with e.g. [rothSGA::check_dead_strains] and
#' [rothSGA::check_self_crosses]
#'
#' This pipeline can be quickly generated using [rothSGA::new_batch_report]
#' which generates an Rmarkdown report from a standard template.
#' @md
#' @name rothSGA
NULL
