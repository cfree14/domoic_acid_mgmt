

# Set dimensions of simulation grid
# ymin=42; ymax=46; yres=0.05; ndays=180
set_grid <- function(ymin, ymax, yres, ndays){

  # Build matrix
  xvals <- 1:ndays
  yvals <- seq(ymin, ymax, yres)
  grid_mat <- matrix(data=NA, nrow=length(yvals), ncol=ndays, dimnames=list(yvals, xvals))

  # Return grid
  return(grid_mat)

}
