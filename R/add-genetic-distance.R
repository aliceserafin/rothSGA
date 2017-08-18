#' Creates a distance column containing the genetic distance between the array gene and the query gene
#'
#' doi:  10.1016/S0076-6879(10)70009-4
#'
#' @param df A data.frame with strain_id and query_id columns
#'
#' @md
#' @export

add_genetic_distance <- function(df) {
  db <- rothfreezer::src_rothfreezer()

  queries <- unique(df$query_id)
  query_gene_ids <-
    tbl(db, 'strains') %>%
    filter(strain_id %in% queries) %>%
    select(query_id = strain_id, query_gene_id = gene_id) %>%
    collect()
  query_genes <- unique(query_gene_ids$query_gene_id)
  array_genes <- unique(df$gene_id)

  distances <-
    tbl(db, 'genetic_distances') %>%
    select(gene_id_a, gene_id_b, distance) %>%
    filter((gene_id_a %in% array_genes) & (gene_id_b %in% query_genes)) %>%
    collect()

  df %>%
    left_join(query_gene_ids, by = 'query_id') %>%
    left_join(distances, by = c('gene_id' = 'gene_id_a', 'query_gene_id' = 'gene_id_b'))
}
