
# Quantify management performance
quantify_performance <- function(results){

  # Build stats
  stats <- results$mgmt_grid %>%
    # Remove pre-season days
    filter(day>=7) %>%
    # Calculate stats
    group_by(status_diff) %>%
    summarize(n=n()) %>%
    mutate(prop=n/sum(n)) %>%
    ungroup()

  # Return
  return(stats)

}
