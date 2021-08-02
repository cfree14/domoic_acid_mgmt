quantify_performance <- function(results){

  # Build stats
  stats <- results$mgmt_grid %>%
    group_by(status_diff) %>%
    summarize(n=n()) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup()

  # Return
  return(stats)

}
