
# Convert raster to dataframe for plotting
convert_raster <- function(ras){

  # Build matrix
  ras_df <- ras %>%
    raster::as.data.frame(xy=T) %>%
    rename(day=x, lat_dd=y, p_over=layer)

  # Return grid
  return(ras_df)

}
