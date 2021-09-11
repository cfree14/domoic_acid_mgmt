
# Simulate management
# test_schedule="once"; ncrabs=6; close_thresh=1/6; nclean_tests=2; test_interval=1
simulate_mgmt <- function(toxin_grid, stations, mgmt_zones,
                          test_schedule="once", ncrabs=6, close_thresh=1/6, nclean_tests=2, test_interval=1, plot=F){

  # Extract parameters
  ndays <- ncol(toxin_grid)

  # Convert toxin grid to dataframe
  toxin_grid_df <- toxin_grid %>%
    as.data.frame(xy=T) %>%
    rename(day=x, lat_dd=y, pover=layer)

  # Sample schedule
  sample_dates <- seq(1, ndays, test_interval*7)

  # Build sample container
  survey_results_mat <- expand.grid(day=sample_dates,
                                    station_id=1:length(stations)) %>%
    # Add station latitude
    mutate(lat_dd=stations[station_id]) %>%
    # Arrange
    select(station_id, lat_dd, day) %>%
    arrange(station_id, lat_dd, day)

  # Extract true proportions on a sample data
  pover_true <- raster::extract(x=toxin_grid, y=survey_results_mat[, 3:2])

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
    mutate(pover_true=pover_true,
           status_true=ifelse(pover_true<=close_thresh, "clean", "contaminated")) %>%
    # Sample from true proportions
    rowwise() %>%
    mutate(pover_obs=conduct_sampling(prob=pover_true, nsample=ncrabs)) %>%
    ungroup() %>%
    # Make management decisions
    mutate(status_obs=ifelse(pover_obs<=close_thresh, "clean", "contaminated"))

  # Plot check
  # plot(pover_obs ~ pover_true, survey_results)

  # Plot full surveys
  if(F){

    # Plot grid and sampling
    g <- ggplot(toxin_grid_df, aes(x=day, y=lat_dd, fill=pover)) +
      geom_raster() +
      # Surveys
      geom_point(data=survey_results_full, mapping=aes(x=day, y=lat_dd, shape=status_obs), inherit.aes = F) +
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
    select(lat_dd, day, everything()) %>%
    arrange(lat_dd, day) %>%
    # Add sample number
    group_by(lat_dd) %>%
    mutate(sample_id=1:n()) %>%
    ungroup() %>%
    select(lat_dd, sample_id, everything()) %>%
    # Count number of consecutive days
    group_by(lat_dd) %>%
    mutate(rle=sequence(rle(x=status_obs)$lengths)) %>%
    ungroup() %>%
    # Find last week
    group_by(lat_dd) %>%
    mutate(day_last=ifelse(length(day[status_obs=="clean" & rle==2]) > 0,
                           day[status_obs=="clean" & rle==2] %>% min(), max(day))) %>%
    ungroup() %>%
    # Reduce to only weeks used
    group_by(lat_dd) %>%
    filter(day<=day_last) %>%
    ungroup()

  # Plot pared surveys
  if(F){

    # Plot grid and sampling
    g <- ggplot(toxin_grid_df, aes(x=day, y=lat_dd, fill=pover)) +
      geom_raster() +
      # Surveys
      geom_point(data=survey_results, mapping=aes(x=day, y=lat_dd, shape=status_obs), inherit.aes = F) +
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
    group_by(station_id, lat_dd) %>%
    summarize(open_date=max(day)) %>%
    ungroup()

  # Build management grid
  mgmt_grid_df <- toxin_grid_df %>%
    # Add management zone
    mutate(mgmt_zone=cut(lat_dd, breaks=mgmt_zones, labels=1:length(stations)) %>% as.numeric()) %>%
    # Add true status
    mutate(status_true=ifelse(pover<=close_thresh, "Open", "Closed")) %>%
    # Arrange
    select(mgmt_zone, lat_dd, day, pover, status_true) %>%
    # Add open date
    left_join(open_dates %>% select(-lat_dd), by=c("mgmt_zone"="station_id")) %>%
    # Add actual status
    group_by(mgmt_zone) %>%
    mutate(status_used=ifelse(day<open_date, "Closed", "Open")) %>%
    ungroup() %>%
    # Classify performance
    mutate(status_diff=paste(status_true, status_used, sep="-"),
           status_diff=recode_factor(status_diff,
                                     "Open-Open"="Open correctly",
                                     "Closed-Closed"="Closed correctly",
                                     "Open-Closed"="Closed unnecessarily",
                                     "Closed-Open"="Open recklessly"))


  # Plot management grid
  if(plot==T){

    # # Format
    # mgmt_grid_df2 <- mgmt_grid_df %>%
    #   select(lat_dd, day, status_true, status_used) %>%
    #   gather(key="scenario", value="status", 3:4) %>%
    #   mutate(scenario=recode_factor(scenario,
    #                          "status_true"="Perfect management",
    #                          "status_used"="Actual management",
    #                          "status_diff"="Management performance"))
    #
    # # Plot
    # g1 <- ggplot(mgmt_grid_df2, aes(x=day, y=lat_dd, fill=status)) +
    #   facet_wrap(~scenario, ncol=1) +
    #   geom_raster() +
    #   # Labels
    #   labs(x="Day", y="Latitude (°N)", title="Management status") +
    #   # Theme
    #   theme_bw()
    # g1

    # Plot
    g2 <- ggplot(mgmt_grid_df, aes(x=day, y=lat_dd, fill=status_diff)) +
      geom_raster() +
      # Labels
      labs(x="Day", y="Latitude (°N)", title="Management performance") +
      # Legend
      scale_fill_discrete(name="Management") +
      # Theme
      theme_bw()
    # g2
    print(g2)

    # Merge
    # g <- gridExtra::grid.arrange(g1, g2, ncol=1)

  }

  # Return
  output <- list(survey_results=survey_results, mgmt_grid=mgmt_grid_df)
  return(output)

}
