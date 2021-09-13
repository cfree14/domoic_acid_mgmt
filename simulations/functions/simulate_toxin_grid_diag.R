
# Test function
# out1a <- simulate_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=90, last_day_bot=40)
# out1b <- simulate_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=120, last_day_bot=20, plot=T)
# out1c <- simulate_toxin_grid_diag(prop_top=0.6, prop_bot=0.2, last_day_top=90, last_day_bot=60, plot=T)

# Simulate toxin grid
simulate_toxin_grid_diag <- function(span, prop_top, prop_bot, last_day_top, last_day_bot, plot=T){

  # Setup grid
  lat_s <- 42
  lat_n <- 46
  lat_by <- 0.1
  ndays <- 256 + 7
  lats <- seq(lat_s, lat_n, lat_by)
  nlats <- length(lats)
  days <- 1:ndays
  grid_df_xy <- expand.grid(day=days, lat=lats) %>%
    arrange(day, lat)

  # Determine lat where bloom begins
  lat1 <- lat_n - span
  lat1_use <- lats[which.min(abs(lat1-lats))]
  lats_bloom <- lats[lats>=lat1_use]
  lats_no_bloom <- lats[lats<lat1_use]
  prop_init_bloom <- seq(prop_bot, prop_top, length.out = length(lats_bloom))
  prop_init_no_bloom <- rep(0, length(lats_no_bloom))
  prop_inits <- c(prop_init_no_bloom, prop_init_bloom)

  # Calculate initial PCONTAM at each latitudinal band
  props_init_df <- tibble(lat=lats,
                          prop_init=prop_inits)

  # Calculate day with 0 at each latitudinal band
  slope <- (lat_n-lat1_use) / (last_day_top-last_day_bot)
  intercept <- lat_n - slope * last_day_top
  days <- (lats - intercept) /slope
  day_when0_df <- tibble(lat=lats,
                         #prop=0,
                         day_zero=days %>% ceiling()) %>%
    mutate(day_zero=ifelse(lat<lat1_use, 1, day_zero))

  # Quick plot check
  # plot(lat~day_zero, data=day_when0_df, xlim=c(0,ndays), ylim=c(lat_s, lat_n))

  # Build data
  grid_df <- grid_df_xy %>%
    # Add initial prop
    left_join(props_init_df, by="lat") %>%
    # Add day when 0
    left_join(day_when0_df, by="lat") %>%
    # Arrange
    arrange(lat, day) %>%
    # Linearly interpolate
    group_by(lat) %>%
    mutate(slope = (0-prop_init)/(day_zero-1),
           intercept=prop_init-slope,
           prop=slope*day+intercept,
           prop=ifelse(prop<0 | is.na(prop), 0, prop)) %>%
    ungroup() %>%
    # Simplify
    select(day, lat, prop)

  # If plot
  if(plot){

    # Reference points
    ref_pts <- tibble(day=c(last_day_bot, last_day_top),
                      lat=c(min(lat1_use), max(lat_n)))

    # Plot grid
    g <- ggplot(grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      # Contour lines
      geom_contour(data=grid_df,
                   mapping=aes(x=day, y=lat, z=prop),
                   breaks=seq(0,1,0.1), color="black") +
      # Zero points/lines
      geom_point(data=ref_pts, mapping=aes(x=day, y=lat), inherit.aes = F) +
      geom_line(data=ref_pts, mapping=aes(x=day, y=lat), inherit.aes = F, linetype="dotted") +
      # Labels
      labs(x="Day", y="Latitude (°N)") +
      # Legend
      scale_fill_gradientn(name="Proportion above\naction threshhold",
                           na.value = "white",
                           lim=c(0,1),
                           colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw() + theme(legend.position = "bottom")
    print(g)

  }

  # Return
  return(grid_df)

}


