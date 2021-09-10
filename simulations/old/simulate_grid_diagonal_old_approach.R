
# Packages
library(tidyverse)

# Diagonal
####################################################################################

# Function to simulate a diaganol toxin grid
# prop_top <- 0.9; prop_bot <- 0.4; last_day_top <- 90; last_day_bot <- 40; plot <- T
# sim_toxin_grid_diag_old <- function(prop_top, prop_bot, last_day_top, last_day_bot, plot=T){
#
#   # Setup grid
#   lat1 <- 40
#   lat2 <- 50
#   lat_by <- 0.1
#   ndays <- 180
#   lats <- seq(lat1, lat2, lat_by)
#   days <- 1:ndays
#   grid_df <- expand.grid(day=days, lat=lats) %>%
#     arrange(day, lat)
#
#   # Function to calculate limit props
#   # last_day <- last_day_top; init_prop <- prop1_top
#   calc_da_props <- function(init_prop, last_day, ndays){
#
#     # Calculate decreasing proportions over threshold
#     # y = mx + b
#     x <- 0:last_day
#     b <- init_prop
#     m <- (0 - init_prop) / (last_day - 0)
#     y <- m*x + b
#     props <- c(y, rep(0, ndays-length(y)))
#
#   }
#
#   # Top
#   last_day_top <- last_day_top
#   prop1_top <- prop_top
#   props_top <- calc_da_props(init_prop=prop1_top, last_day=last_day_top, ndays=ndays)
#   props_top_df <- tibble(limit="Top",
#                          day=1:ndays,
#                          prop=props_top)
#
#   # Bottom
#   last_day_bottom <- last_day_bot
#   prop1_bottom <- prop_bot
#   props_bottom <- calc_da_props(init_prop=prop1_bottom, last_day=last_day_bottom, ndays=ndays)
#   props_bot_df <- tibble(limit="Bottom",
#                          day=1:ndays,
#                          prop=props_bottom)
#
#   # Merge
#   limits_df <- bind_rows(props_bot_df, props_top_df)
#
#   # Plot check
#   if(F){
#     g <- ggplot(limits_df, aes(x=day, y=prop, color=limit)) +
#       geom_line() +
#       # Reference line
#       geom_hline(yintercept = 1/6, linetype="dotted") +
#       # Labels
#       labs(x="Day", y="Proportion of crabs\nabove the action threshhold") +
#       # Limits
#       lims(y=c(0,1)) +
#       # Legends
#       scale_color_discrete(name="Grid limit") +
#       # Theme
#       theme_bw()
#     g
#   }
#
#   # Build grid through interpolation
#   grid_df <- purrr::map_df(1:ndays, function(x){
#
#     # Top
#     prop_top <- limits_df %>%
#       filter(limit=="Top" & day==x) %>% pull(prop)
#
#     # Bottom
#     prop_bot <- limits_df %>%
#       filter(limit=="Bottom" & day==x) %>% pull(prop)
#
#     # Interpolate
#     props <- seq(prop_bot, prop_top, length.out=length(lats))
#
#     # Create dataframe
#     day_df <- tibble(day=x,
#                      lat=lats,
#                      prop=props)
#
#
#   })
#
#   # If plot
#   if(plot){
#
#     # Reference points
#     ref_pts <- tibble(day=c(last_day_bottom, last_day_top),
#                       lat=c(min(lats), max(lats)))
#
#     # Plot grid
#     g <- ggplot(grid_df, aes(x=day, y=lat, fill=prop)) +
#       geom_raster() +
#       # Contour lines
#       geom_contour(data=grid_df,
#                    mapping=aes(x=day, y=lat, z=prop),
#                    breaks=seq(0,1,0.1), color="black") +
#       # Zero points/lines
#       geom_point(data=ref_pts, mapping=aes(x=day, y=lat), inherit.aes = F) +
#       geom_line(data=ref_pts, mapping=aes(x=day, y=lat), inherit.aes = F, linetype="dotted") +
#       # Labels
#       labs(x="Day", y="Latitude (°N)") +
#       # Legend
#       scale_fill_gradientn(name="Proportion above\naction threshhold",
#                            na.value = "white",
#                            lim=c(0,1),
#                            colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
#       guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#       # Theme
#       theme_bw() + theme(legend.position = "bottom")
#     g
#     print(g)
#
#   }
#
#   # Return
#   return(grid_df)
#
# }
#
# # Test function
# out1a <- sim_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=90, last_day_bot=40)
# out1b <- sim_toxin_grid_diag(prop_top=0.9, prop_bot=0.4, last_day_top=120, last_day_bot=20, plot=T)
# out1c <- sim_toxin_grid_diag(prop_top=0.6, prop_bot=0.2, last_day_top=90, last_day_bot=60, plot=T)



# Diagonal new
####################################################################################

# Function to simulate a diaganol toxin grid
# prop_top <- 0.9; prop_bot <- 0.4; last_day_top <- 90; last_day_bot <- 40; plot <- T





