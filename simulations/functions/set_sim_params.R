
# Read parameter key
param_key <- readxl::read_excel(path="tables/TableS6_simulated_bloom_design.xlsx", sheet="Raw")

# Setup simulation parameters
# scenario <- "large"
set_sim_params <- function(scenario, param_key=param_key){

  # Parameter extraction
  #######################################

  # Extract span parameters
  span_lo <- param_key$span_lo[param_key$size==scenario]
  span_hi <- param_key$span_hi[param_key$size==scenario]

  # Extract top parameters
  prop_top_lo <- param_key$prop_top_lo[param_key$size==scenario]
  prop_top_hi <- param_key$prop_top_hi[param_key$size==scenario]
  last_day_top_lo <- param_key$last_day_top_lo[param_key$size==scenario]
  last_day_top_hi <- param_key$last_day_top_hi[param_key$size==scenario]

  # Extract bottom parameters
  prop_bot_lo <- param_key$prop_bot_lo[param_key$size==scenario]
  prop_bot_hi <- param_key$prop_bot_hi[param_key$size==scenario]
  last_day_bot_lo <- param_key$last_day_bot_lo[param_key$size==scenario]
  last_day_bot_hi <- param_key$last_day_bot_hi[param_key$size==scenario]

  # Extract epicenter parameters
  center_prop_lo <- param_key$center_prop_lo[param_key$size==scenario]
  center_prop_hi <- param_key$center_prop_hi[param_key$size==scenario]
  center_x_lo <- param_key$center_x_lo[param_key$size==scenario]
  center_x_hi <- param_key$center_x_hi[param_key$size==scenario]
  center_y_lo <- param_key$center_y_lo[param_key$size==scenario]
  center_y_hi <- param_key$center_y_hi[param_key$size==scenario]

  # Extract span parameters
  span_x_lo <- param_key$span_x_lo[param_key$size==scenario]
  span_x_hi <- param_key$span_x_hi[param_key$size==scenario]
  span_y_lo <- param_key$span_y_lo[param_key$size==scenario]
  span_y_hi <- param_key$span_y_hi[param_key$size==scenario]


  # Parameter simulation
  #######################################

  # Calculate span
  span <- runif(1, min=span_lo, max=span_hi)

  # Calculate proportions
  prop_top <- runif(1, min=prop_top_lo, max=prop_top_hi)
  prop_bot_perc <- runif(1, min=prop_bot_lo, max=prop_bot_lo)
  prop_bot <- prop_top * prop_bot_perc

  # Calculate last days
  last_day_top <- runif(1, min=last_day_top_lo, max=last_day_top_hi) %>% ceiling()
  last_day_bot_perc <- runif(1, min=last_day_bot_lo, max=last_day_bot_hi)
  last_day_bot <- (last_day_top * last_day_bot_perc) %>% ceiling()

  # Calculate seed values
  center_prop <- runif(1, min=center_prop_lo, max=center_prop_hi)
  center_x <- runif(1, min=center_x_lo, max=center_x_hi)
  center_y <- runif(1, min=center_y_lo, max=center_y_hi)
  span_x <- runif(1, min=span_x_lo, max=span_x_hi)
  span_y <- runif(1, min=span_y_lo, max=span_y_hi)

  # Package output
  output <- list(scenario=scenario,
                 span=span,
                 prop_top=prop_top,
                 prop_bot=prop_bot,
                 last_day_top=last_day_top,
                 last_day_bot=last_day_bot,
                 center_prop=center_prop,
                 center_x=center_x,
                 center_y=center_y,
                 span_x=span_x,
                 span_y=span_y)

  # Return
  return(output)

}
