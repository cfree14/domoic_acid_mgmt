
# Read parameter key
param_key <- readxl::read_excel(path="tables/TableS7_simulated_bloom_design.xlsx", sheet="Raw")

# Setup simulation parameters
# scenario <- "small"
set_sim_params <- function(scenario, param_key=param_key){

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

  # Calculate proportions
  prop_top <- runif(1, min=prop_top_lo, max=prop_top_hi)
  prop_bot_perc <- runif(1, min=prop_bot_lo, max=prop_bot_lo)
  prop_bot <- prop_top * prop_bot_perc

  # Calculate last days
  last_day_top <- runif(1, min=last_day_top_lo, max=last_day_top_hi) %>% ceiling()
  last_day_bot_perc <- runif(1, min=last_day_bot_lo, max=last_day_bot_hi)
  last_day_bot <- (last_day_top * last_day_bot_perc) %>% ceiling()

  # Package output
  output <- list(scenario=scenario,
                 prop_top=prop_top,
                 prop_bot=prop_bot,
                 last_day_top=last_day_top,
                 last_day_bot=last_day_bot)

  # Return
  return(output)

}
