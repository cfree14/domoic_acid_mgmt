
make_mgmt_decisions_init_only <- function(survey_results, mgmt_zones, toxin_grid_df){

  # Opening dates
  open_dates <- survey_results %>%
    group_by(station_id, lat) %>%
    summarize(open_date=max(day)) %>%
    ungroup()

  # Build management grid
  mgmt_zones_padded <- c(mgmt_zones[1]-1, mgmt_zones[2:length(mgmt_zones)])
  mgmt_grid_df <- toxin_grid_df %>%
    # Add management zone
    mutate(mgmt_zone=cut(lat, breaks=mgmt_zones_padded, labels=1:length(stations)) %>% as.numeric()) %>%
    # Add true status
    mutate(status_true=ifelse(prop<=close_thresh, "Open", "Closed")) %>%
    # Arrange
    select(mgmt_zone, lat, day, prop, status_true) %>%
    # Add open date
    left_join(open_dates %>% select(-lat), by=c("mgmt_zone"="station_id")) %>%
    # Add actual status
    group_by(mgmt_zone) %>%
    mutate(status_used=ifelse(day<open_date, "Closed", "Open")) %>%
    ungroup() %>%
    # Classify performance
    mutate(correct_yn=status_true==status_used,
           status_diff=paste(status_true, status_used, sep="-"),
           status_diff=recode_factor(status_diff,
                                     "Open-Open"="Open correctly",
                                     "Closed-Closed"="Closed correctly",
                                     "Open-Closed"="Closed unnecessarily",
                                     "Closed-Open"="Open riskily"))

  # Return
  return(mgmt_grid_df)

}
