
# Test function
# out2a <- simulate_toxin_grid_seed(center_val=0.9, center_day=90, center_lat=45, span_days=45, span_lat=2, plot=T)
# out2b <- simulate_toxin_grid_seed(center_val=0.9, center_day=90, center_lat=45, span_days=75, span_lat=2, plot=T)
# out2c <- simulate_toxin_grid_seed(center_val=0.9, center_day=90, center_lat=45, span_days=75, span_lat=4, plot=T)
# out2d <- simulate_toxin_grid_seed(center_val=0.9, center_day=120, center_lat=45, span_days=75, span_lat=4, plot=T)

# Function to simulate a seeded toxin grid
# center_val <- 0.9; center_day <- 90; center_lat <- 45; span_days <- 45; span_lat <- 2; plot=T
simulate_toxin_grid_seed <- function(center_val, center_day, center_lat, span_days, span_lat, plot=T){

  # Setup grid
  lat1 <- 42
  lat2 <- 46
  lat_by <- 0.1
  ndays <- 256
  lats <- seq(lat1, lat2, lat_by)
  days <- 1:ndays
  grid_df <- expand.grid(day=days, lat=lats) %>%
    arrange(day, lat)

  # Seed scenario
  prop_peak <- center_val
  epicenter_x <- center_day
  epicenter_y <- center_lat
  span_x <- span_days
  span_y <- span_lat

  # Span data frames
  span_x_df <- tibble(day=c(epicenter_x-span_x/2, epicenter_x+span_x/2),
                      lat=rep(epicenter_y, 2))
  span_y_df <- tibble(day=rep(epicenter_x, 2),
                      lat=c(epicenter_y-span_y/2, epicenter_y+span_y/2))

  # Long (major) axis is parallel to x-axis
  # (x-h)^2/a^2 + (y-k)^2/b^2 = 1
  h <- epicenter_x
  k <- epicenter_y
  a <- span_x / 2
  b <- span_y / 2

  # Derived values
  x_lo <- h - a
  x_hi <- h + a
  y_lo <- k - a
  y_hi <- k + a

  # For testing:
  #p1 <- 90; p2 <- 44

  # Line function
  line_function <- function(x){
    y <- intercept+slope*x
  }

  # Ellipse function
  ellipse_function <- function(x){
    y <- sqrt( (1 - (x-h)^2/a^2) * b^2) + k
    return(y)
  }

  # Function to dervie distance from center, proportional distance from center, and p(contaminated)
  fcalc_prop2 <- function(pcontam_peak, h, k, a, b, p1, p2){

    # Step 1. Derive D1 (distance from point to centroid)
    p1_scaled <- abs(h-p1)
    p2_scaled <- abs(k-p2)
    d1 <- sqrt((p2_scaled)^2 + (p1_scaled)^2)

    # Step 2. Derive total distance (distance from edge to centroid along radii)
    slope <- (k-p2)/(h-p1)
    theta <- tanh(slope)
    x2 <- 1 / (1/a^2 + slope^2/b^2)
    x <- sqrt(x2)
    y2 <- b^2*(1-x^2/a^2)
    y <- sqrt(y2)
    d <- sqrt(x2 + y2)

    # Step 3. Derive D2 (distance from point to edge)
    d2 <- d - d1

    # Step 4. Derive proportion of point over whole edge
    d_prop <- (1 - d1/d)

    # Step 5. Derive p(contaminated)
    pcontam <- pmax(0, d_prop * pcontam_peak)
    pcontam <- ifelse(is.na(pcontam), pcontam_peak, pcontam)

    # Check via brute force method
    if(F){

      # Find intersection
      x_lo <- h - a
      x_hi <- h + a
      x_intersect <- mosaic::findZeros(ellipse_function(x) - line_function(x) ~ x,
                                       xlim=range(x_lo, x_hi) ) %>% as.numeric()
      y_intersect <- ellipse_function(x=x_intersect) %>% as.numeric()

      # Find distance
      d_check <- sqrt((h-x_intersect)^2 + (k-y_intersect)^2)

    }

    # Plot check
    if(F){

      # Derive values to plot
      x_vals <- seq(x_lo, x_hi, length.out=1000)
      y <- ellipse_function(x=x_vals)
      ellipse_df <- tibble(x=c(x_vals, rev(x_vals)),
                           y=c(y, k-(y-k)))

      # Plot
      g <- ggplot() +
        # Plot span
        geom_line(data=span_x_df, mapping=aes(x=day, y=lat), inherit.aes = F) +
        geom_line(data=span_y_df, mapping=aes(x=day, y=lat), inherit.aes = F) +
        # Plot epicenter
        geom_point(mapping=aes(x=h, y=k), size=2) +
        # Plot line
        geom_abline(slope=slope, intercept = intercept, color="red", linetype="dotted") +
        # Plot intersection
        geom_point(mapping=aes(x=x_intersect, y=y_intersect), color="red", size=2) +
        # Plot point
        geom_point(mapping=aes(x=p1, y=p2), size=2, color="red") +
        # Plot ellipse
        geom_path(data=ellipse_df, mapping=aes(x=x, y=y), inherit.aes = F) +
        # Labels
        labs(x="Day", y="Latitude (°N)") +
        # Theme
        theme_bw() + theme(legend.position = "bottom")
      g

    } # end plot

    # Return
    return(pcontam)

  }

  # Prepare elliptical epicenter
  grid_df <- grid_df %>%
    # Clip to values of interest
    # filter(day>=x_lo & day <=x_hi) %>%
    # filter(lat>=y_lo & lat <= y_hi) %>%
    # Compute proportional distance from centroid
    rowwise() %>%
    mutate(prop=fcalc_prop2(pcontam_peak=center_val, h=h, k=k, a=a, b=b, p1=day, p2=lat)) %>%
    ungroup()

  # If plotting
  if(plot){

    # Derive values to plot
    x_lo <- h - a
    x_hi <- h + a
    x_vals <- seq(x_lo, x_hi, length.out=1000)
    y <- ellipse_function(x=x_vals)
    ellipse_df <- tibble(x=c(x_vals, rev(x_vals)),
                         y=c(y, k-(y-k)))

    # Plot grid
    g <- ggplot(grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      # Plot contours
      # geom_contour(data=grid_df,
      #              mapping=aes(x=day, y=lat, z=prop),
      #              breaks=seq(0,1,0.1), color="black", alpha=0.2) +
      # Plot span
      geom_line(data=span_x_df, mapping=aes(x=day, y=lat), inherit.aes = F) +
      geom_line(data=span_y_df, mapping=aes(x=day, y=lat), inherit.aes = F) +
      # Plot epicenter
      geom_point(x=epicenter_x, y=epicenter_y) +
      # Plot ellipse
      geom_path(data=ellipse_df, mapping=aes(x=x, y=y), linetype="dotted", inherit.aes = F) +
      # Labels
      labs(x="Day", y="Latitude (°N)") +
      # Legend
      scale_fill_gradientn(name="Proportion above\naction threshhold",
                           na.value = "white",
                           colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw() + theme(legend.position = "bottom")
    print(g)

  }

  # Return
  return(grid_df)

}
