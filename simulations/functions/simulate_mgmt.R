
# Simulate management
# toxin_test_schedule="once"; ncrabs=6; close_thresh=1/6; nclean_tests=2; test_interval=1
simulate_mgmt <- function(toxin_grid_df, stations, mgmt_zones,
                          test_schedule="once", ncrabs=6, close_thresh=1/6, nclean_tests=2, test_interval=1, plot=F){

  # Extract parameters
  ndays <- toxin_grid_df$day %>% max()
  grid_lats <- sort(unique(toxin_grid_df$lat))

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

  # Function to conduct sampling
  conduct_sampling <- function(prob, nsample){
    x <- runif(n=nsample, min=0, max=1)
    nover <- sum(x <= prob)
    pover <- nover / nsample
    return(pover)
  }

  # Expand container
  survey_results_full <- survey_results_mat %>%
    # Add true proportions to container
    left_join(toxin_grid_df, by=c('day', "lat_grid"="lat")) %>%
    rename(pover_true=prop) %>%
    mutate(status_true=ifelse(pover_true<=close_thresh, "clean", "contaminated")) %>%
    # Sample from true proportions
    rowwise() %>%
    mutate(pover_obs=conduct_sampling(prob=pover_true, nsample=ncrabs)) %>%
    ungroup() %>%
    # Make management decisions
    mutate(status_obs=ifelse(pover_obs<=close_thresh, "clean", "contaminated"))

  # Plot check
  # plot(pover_obs ~ pover_true, survey_results_full)

  # Plot full surveys
  if(F){

    # Plot grid and sampling
    g <- ggplot(toxin_grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      # Surveys
      geom_point(data=survey_results_full, mapping=aes(x=day, y=lat, shape=status_obs), inherit.aes = F) +
      # Labels
      labs(x="Day", y="Latitude (°N)", title="All surveys (unpared)") +
      # Legend
      scale_fill_gradientn(name="True proportion of crabs\nabove action threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      scale_shape_manual(name="Estimated status", values=c(21, 16)) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw()
    g

  }

  # Pare back sampling based on management and record management
  survey_results <-  survey_results_full %>%
    # Arrange
    select(lat, day, everything()) %>%
    arrange(lat, day) %>%
    # Add sample number
    group_by(lat) %>%
    mutate(sample_id=1:n()) %>%
    ungroup() %>%
    select(lat, sample_id, everything()) %>%
    # Count number of consecutive days
    group_by(lat) %>%
    mutate(rle=sequence(rle(x=status_obs)$lengths)) %>%
    ungroup() %>%
    # Find last week
    group_by(lat) %>%
    mutate(day_last=ifelse(length(day[status_obs=="clean" & rle==2]) > 0,
                           day[status_obs=="clean" & rle==2] %>% min(), max(day))) %>%
    ungroup() %>%
    # Reduce to only weeks used
    group_by(lat) %>%
    filter(day<=day_last) %>%
    ungroup()

  # Plot pared surveys
  if(F){

    # Plot grid and sampling
    g <- ggplot(toxin_grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      # Surveys
      geom_point(data=survey_results, mapping=aes(x=day, y=lat, shape=status_obs), inherit.aes = F) +
      # Labels
      labs(x="Day", y="Latitude (°N)", title="Surveys (pared)") +
      # Legend
      scale_fill_gradientn(name="True proportion of crabs\nabove action threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      scale_shape_manual(name="Estimated status", values=c(21, 16)) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw()
    g

  }

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


  # Plot management grid
  if(plot==T){

    # Plot monitoring
    g1 <- ggplot(toxin_grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      # Surveys
      geom_point(data=survey_results, mapping=aes(x=day, y=lat, shape=status_obs), inherit.aes = F) +
      # Labels
      labs(x="Day", y="Latitude (°N)", title="Monitoring results") +
      # Legend
      scale_fill_gradientn(name="True proportion of crabs\nabove action threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      scale_shape_manual(name="Estimated status", values=c(21, 16)) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw()

    # Plot managment
    g2 <- ggplot(mgmt_grid_df, aes(x=day, y=lat, fill=status_used, alpha=correct_yn)) +
      geom_raster() +
      # Labels
      labs(x="Day", y="Latitude (°N)", title="Management performance") +
      # Legend
      scale_fill_discrete(name="Management") +
      scale_alpha_manual(name="Action correct?", values=c(0.5, 1)) +
      # Theme
      theme_bw()

    # Merge
    g <- gridExtra::grid.arrange(g1, g2, ncol=1)
    print(g)

  }

  # Return
  output <- list(survey_results=survey_results, mgmt_grid=mgmt_grid_df)
  return(output)

}
