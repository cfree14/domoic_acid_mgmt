
# Set sampling stations
# nstations=3; grid=grid_temp
set_mgmt_even <- function(nstations, grid){

  # Derive stations
  ymin <- min(rownames(grid)) %>% as.numeric()
  ymax <- max(rownames(grid)) %>% as.numeric()
  zone_vals <- seq(ymin, ymax, length.out=nstations+1)
  station_vals <- zoo::rollmean(zone_vals, k=2)

  # Convert grid to dataframe for plotting
  grid_df <- grid %>%
    as.data.frame() %>%
    rownames_to_column(var="lat_dd") %>%
    gather(key="day", value="value", 2:ncol(.)) %>%
    mutate_all(as.numeric)

  # Plot
  yvals_df <- tibble(day=1, lat_dd=station_vals)
  g <- ggplot(grid_df, aes(x=day, y=lat_dd)) +
    geom_raster(fill="grey90") +
    # Stations
    geom_point(data=yvals_df, pch=16, size=2) +
    geom_hline(yintercept=station_vals, linetype="dotted") +
    geom_hline(yintercept=zone_vals, linetype="solid") +
    # Labels
    labs(x="Day", y="Latitude (°N)", title=paste(nstations, " stations")) +
    # Theme
    theme_bw()
  print(g)

  # Return
  output <- list(stations=station_vals, zones=zone_vals)
  return(output)

}
