
# Set sampling stations
# nstations=3; ymin <- 42; ymax <- 46, ndays <- 256
set_mgmt_even <- function(nstations, ymin, ymax, ndays, plot=F){

  # Derive stations
  zone_vals <- seq(ymin, ymax, length.out=nstations+1)
  station_vals <- zoo::rollmean(zone_vals, k=2)

  # Plot
  if(plot==T){
    yvals_df <- tibble(day=1, lat_dd=station_vals)
    g <- ggplot() +
      # Stations
      geom_hline(yintercept=station_vals, linetype="dotted") +
      geom_hline(yintercept=zone_vals, linetype="solid") +
      # Labels
      labs(x="Day", y="Latitude (°N)", title=paste(nstations, " stations")) +
      # Limits
      lims(x=c(1, ndays), y=c(ymin, ymax)) +
      # Theme
      theme_bw()
    print(g)
  }

  # Return
  output <- list(stations=station_vals, zones=zone_vals)
  return(output)

}
