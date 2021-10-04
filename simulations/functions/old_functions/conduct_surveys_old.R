
conduct_surveys <- function(survey_results_full, repeat_interval){

  # Loop through lats
  lats <- sort(unique(survey_results_full$lat))
  days <- sort(unique(survey_results_full$day))
  for(i in 1:length(lats)){

    # Get lat results
    lat_do <- lats[i]
    lat_results <- survey_results_full %>%
      filter(lat==lat_do) %>% pull(status_obs)

    # Establish day stats and determine whether it gets used
    j <- 1
    day_next <- days[j]
    for(j in 1:length(days)){

      # Day info
      day_do <- days[j]
      day_result <- lat_results[j]

      # Were the last two clean?
      last_two_results <- lat_results[c(j-1,j)]
      last_two_clean <- sum(last_two_results== "clean") == 2

      # Are we sampling today?
      # If yes, record results and schedule next sample
      if(day_do==day_next){

        # Sample
        use <- T

        # If the 1st sample is clean or the last two sample were clean, schedule for future
        if( (day_do==1 & day_result=="clean") | (day_do>1 & last_two_clean==T) ){
          day_next <- day_do + repeat_interval*7
        # Otherwise, schedule for next week
        }else{
          day_next <- days[j+1]
        }

      # If not sampling today
      }else{
        use <- F
      }

      # Assemble results
      df <- tibble(lat=lat_do,
                   day=day_do,
                   status_obs=day_result,
                   next_survey_day=day_next,
                   use=use)

      # Merge data
      if(j==1){ldata <- df}else{ldata <- bind_rows(ldata, df)}

    }

    # Merge
    if(i==1){data_out <- ldata}else{data_out <- bind_rows(data_out, ldata)}

  }

  # Filter
  data_use <- data_out %>%
    filter(use==T)

  # Return
  return(data_use)

}
