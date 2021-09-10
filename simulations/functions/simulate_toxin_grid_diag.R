
# Test function
# out1a <- simulate_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=90, last_day_bot=40)
# out1b <- simulate_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=120, last_day_bot=20, plot=T)
# out1c <- simulate_toxin_grid_diag(prop_top=0.6, prop_bot=0.2, last_day_top=90, last_day_bot=60, plot=T)

# Simulate toxin grid
simulate_toxin_grid_diag <- function(prop_top, prop_bot, last_day_top, last_day_bot, plot=T){

  # Setup grid
  lat1 <- 42
  lat2 <- 46
  lat_by <- 0.1
  ndays <- 256
  lats <- seq(lat1, lat2, lat_by)
  nlats <- length(lats)
  days <- 1:ndays
  grid_df_xy <- expand.grid(day=days, lat=lats) %>%
    arrange(day, lat)

  # Calculate initial PCONTAM at each latitudinal band
  props_init_df <- tibble(lat=lats,
                          prop_init=seq(prop_bot, prop_top, length.out = nlats))

  # Calculate day with 0 at each latitudinal band
  slope <- (lat2-lat1) / (last_day_top-last_day_bot)
  intercept <- lat2 - slope * last_day_top
  days <- (lats - intercept) /slope
  day_when0_df <- tibble(lat=lats,
                         #prop=0,
                         day_zero=ceiling(days))

  # Quick plot check
  # plot(lat~day, data=day_when0_df, xlim=c(0,ndays), ylim=c(lat1, lat2))

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
           prop=ifelse(prop<0, 0, prop)) %>%
    ungroup() %>%
    # Simplify
    select(day, lat, prop)

  # If plot
  if(plot){

    # Reference points
    ref_pts <- tibble(day=c(last_day_bot, last_day_top),
                      lat=c(min(lats), max(lats)))

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


