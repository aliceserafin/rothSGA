#' Creates different figures with ggplot2 to visualize the data and the quality controls steps.
#'
#'
#' @param sm_data A screenmill dataset (see [screenmill::read_screenmill])
#' @param path where the figures will be generated. A figures directory will be created.
#' @param normalized Logical. Has the data been spatially and plate normalized ? default is normalized = TRUE (see[rothSGA::apply_spatial_effect_normalization] and [rothSGA::apply_plate_normalization])
#'
#' @md
#' @export


plot_sm_data <- function(sm_data, normalized = TRUE, path) {


  assertthat::assert_that(assertthat::is.writeable(path))


  if (!dir.exists('figures')) {
  dir.create('figures')
  }

  sm_data %>%
    ggplot(aes(x= colony_col, y=colony_row, fill=size)) +
    geom_tile(color="white") +
    coord_fixed(expand = F) +
    facet_wrap(~ plate_id) +
    scale_y_reverse() +
    scale_fill_gradient2(midpoint =  mean(results$size)) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = 'green')
    ) +
    ggsave('figures/raw_plates_heatmaps.pdf', width = 10.8, height = 9.64)


  queries <-
    sm_data %>%
    group_by(plate_id, query_name, cisplatin, bio_replicate) %>%
    summarise(query = mean(size[strain_name == 'his3'], trim = 0.2)) %>%
    ungroup()

  queries %>%
    ggplot(aes(x = cisplatin, y = query, linetype = factor(bio_replicate), color = query_name)) +
    geom_line() +
    geom_point() +
    facet_wrap(~query_name, ncol = 5) +
    coord_cartesian(ylim = c(0, 130)) +
    ggsave('figures/Queries_raw_growth.pdf', width = 10.2, height = 7.85)


  if (normalized == TRUE) {

    # Spatial effect visualisation

    sm_data %>%
      ggplot(aes(x= colony_col, y=colony_row, fill=spatial_effect)) +
      geom_tile(color="white") +
      coord_fixed(expand = F) +
      facet_wrap(~ plate_id) +
      scale_y_reverse() +
      scale_fill_gradient2(midpoint = 1) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'green')
      ) +
      ggsave('figures/spatial_effect_models.pdf', width = 10.8, height = 9.64)

    sm_data %>%
      ggplot(aes(x= colony_col, y=colony_row, fill=size_spatial_norm)) +
      geom_tile(color="white") +
      coord_fixed(expand = F) +
      facet_wrap(~ plate_id) +
      scale_y_reverse() +
      scale_fill_gradient2(midpoint =  mean(results$size_spatial_norm)) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'green')
      ) +
      ggsave('figures/spatially_normalized_plates.pdf', width = 10.8, height = 9.64)

    sm_data %>%
      ggplot(aes(x= colony_col, y=colony_row, fill=size_plate_norm)) +
      geom_tile(color="white") +
      coord_fixed(expand = F) +
      facet_wrap(~ plate_id) +
      scale_y_reverse() +
      scale_fill_gradient2(midpoint =  mean(results$size_plate_norm)) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'green')
      ) +
      ggsave('figures/spatially_and_plate_normalized_plates.pdf', width = 10.8, height = 9.64)

  }


}
