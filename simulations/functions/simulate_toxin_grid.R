
# Set sampling stations
# nstations=3; grid=grid_temp
simulate_toxin_grid <- function(grid, type){

  # Grid dimensions
  ndays <- ncol(grid)
  ncells <- nrow(grid)
  ymin <- rownames(grid) %>% as.numeric() %>% min()
  ymax <- rownames(grid) %>% as.numeric() %>% max()
  yres <- (ymax-ymin)/(ncells-1)

  # Uniform linear dissipation
  if(type=="uniform"){
    toxin_grid <-NLMR::nlm_planargradient(ncol=ndays, nrow=ncells, resolution = 1, direction = 90)
  }

  # Diagonal linear dissipation
  if(type=="diagonal"){
    toxin_grid <-NLMR::nlm_planargradient(ncol=ndays, nrow=ncells, resolution = 1, direction = -45)
  }

  # Bulge linear dissipation
  if(type=="bulge"){
    toxin_grid <- NLMR::nlm_distancegradient(ncol=ndays, nrow=ncells, resolution = 1,
                                             origin=c(0, 1, 0, 1)) # xmin, xmax, ymin, ymax
  }

  # Spatially autocorrelated
  if(type=="auto"){
   toxin_grid <- nlm_gaussianfield(ncol=ndays, nrow=ncells, resolution = 1, autocorr_range = 10, mag_var=5, nug=1)
  }

  # Format grid
  grid_extent <- extent(1, ndays, ymin, ymax)
  toxin_grid2 <- toxin_grid %>%
    setExtent(ext=grid_extent)

  # Plot
  g <- plot_grid(toxin_grid2)
  print(g)

  # Return
  return(toxin_grid2)

}
