
# Simulate management
# toxin_grid_df <- toxin_grid_sm; ncrabs=6; close_thresh=1/6; test_interval=1; repeat_interval <- 0; perfect <- T
simulate_mgmt <- function(toxin_grid_df, nstations,
                          ncrabs=6, close_thresh=1/6, test_interval=1,
                          repeat_interval=0, perfect=F, plot=F){

  # Extract grid parameters
  ndays <- max(toxin_grid_df$day)
  ymin <- min(toxin_grid_df$lat)
  ymax <- max(toxin_grid_df$lat)
  grid_lats <- sort(unique(toxin_grid_df$lat))

  # Setup management
  mgmt_info <- set_mgmt_even(nstations=nstations, ymin=ymin, ymax=ymax, ndays=ndays, plot=F)
  stations <- mgmt_info$stations
  mgmt_zones <- mgmt_info$zones

  # Plot stations
  if(F){
    plot_setup(toxin_grid_df, stations, mgmt_zones)
  }

  # Sample schedule
  sample_dates <- seq(1, ndays, test_interval*7)

  # Build sample container
  survey_results_mat <- expand.grid(day=sample_dates,
                                    station_id=1:length(stations)) %>%
    # Add station latitude
    mutate(lat=stations[station_id]) %>%
    # Add the closest lat in the gird
    rowwise() %>%
    mutate(lat_grid=grid_lats[which.min(abs(grid_lats-lat))]) %>%
    ungroup() %>%
    # Arrange
    select(station_id, lat, lat_grid, day) %>%
    arrange(station_id, lat, lat_grid, day)

  # Expand container
  survey_results_full <- survey_results_mat %>%
    # Add true proportions to container
    left_join(toxin_grid_df, by=c('day', "lat_grid"="lat")) %>%
    rename(pover_true=prop) %>%
    mutate(status_true=ifelse(pover_true<=close_thresh, "clean", "contaminated")) %>%
    # Sample from true proportions
    rowwise() %>%
    mutate(pover_obs=conduct_sampling(prob=pover_true, nsample=ncrabs)) %>%
    mutate(pover_obs=ifelse(perfect==T, pover_true, pover_obs)) %>%
    ungroup() %>%
    # If perfect monitoring, overwrite
    # Make management decisions
    mutate(status_obs=ifelse(pover_obs<=close_thresh, "clean", "contaminated"))

  # Plot check
  # plot(pover_obs ~ pover_true, survey_results_full)

  # Plot full surveys
  if(F){
    plot_surveys_all(toxin_grid_df, survey_results_full)
  }

  # Conduct surveys
  survey_results1 <- conduct_surveys(survey_results_full=survey_results_full,
                                     repeat_interval=repeat_interval)

  # Format survey results
  survey_results <- survey_results1 %>%
    # Add station id
    left_join(survey_results_full %>% select(lat, day, station_id), by=c("lat", "day"))

  # Plot pared surveys
  if(F){
    plot_surveys_pared(toxin_grid_df, survey_results)
  }

  # Make management decisions
  mgmt_grid_df <- make_mgmt_decisions(toxin_grid_df,
                                      survey_results, stations, mgmt_zones,
                                      test_interval, close_thresh)

  # Plot management grid
  if(plot==T){
    plot_surveys_mgmt(toxin_grid_df, survey_results, mgmt_grid_df)
  }

  # Return
  output <- list(survey_results=survey_results, mgmt_grid=mgmt_grid_df)
  return(output)

}
