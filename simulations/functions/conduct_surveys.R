
conduct_surveys <- function(survey_results_full, repeat_interval){

  # Loop through lats
  lats <- sort(unique(survey_results_full$lat))
  days <- sort(unique(survey_results_full$day))
  for(i in 1:length(lats)){

    # Lat do
    lat_do <- lats[i]

    # Extract/format results
    ldata <- survey_results_full %>%
      # Lat to do
      filter(lat==lat_do) %>%
      # Simplify
      select(station_id, lat, day, status_obs) %>%
      # Add USE column
      mutate(next_survey_day=NA,
             use=ifelse(day==1, T, NA))

    # Establish day stats and determine whether it gets used
    j <- 1
    day_next <- days[j]
    # for(j in 1:6){
    for(j in 1:length(days)){

      # Day info
      day_do <- days[j]

      # Are we sampling today?
      # If yes, record results and schedule next sample
      if(day_do==day_next){

        # Sample
        use <- T
        ldata$use[j] <- use

        # Parameters
        day_result <- ldata %>% filter(day==day_do) %>% pull(status_obs)

        # Day 1 and clean?
        day1_clean_yn <- (day_do==1 & day_result=="clean")

        # Last two (used) samples 1 week apart and clean?
        last_two_results <- ldata %>%
          # Used surveys
          filter(use==T) %>%
          # Last two surveys
          arrange((desc(day))) %>%
          slice(1:2)
        last_two_days_apart <- ifelse(nrow(last_two_results)==2, diff(last_two_results$day) %>% abs(), 0)
        last_two_1wk_apart <- last_two_days_apart == 7 & nrow(last_two_results) == 2
        last_two_clean <- sum(last_two_results$status_obs=="clean") == 2
        last2_1wk_apart_and_clean_yn <- last_two_1wk_apart & last_two_clean

        # Last sample was a clean follow up sample?
        follow_up_yn <- last_two_days_apart == repeat_interval * 7
        clean_follow_up_yn <- follow_up_yn & day_result == "clean"

        # Schedule for for the future if: (1) 1st sample is clean; (2) last two samples were 1 week apart and clean; or (3) last sample was a clean follow up sample
        if( day1_clean_yn | last2_1wk_apart_and_clean_yn |  clean_follow_up_yn){
          day_next <- day_do + repeat_interval*7
        # Otherwise, schedule for next week
        }else{
          day_next <- days[j+1]
        }

      # If not sampling today
      }else{
        use <- F
        ldata$use[j] <- use
      }

      # Record results
      ldata$next_survey_day[j] <- day_next

    }

    # Merge
    if(i==1){data_out <- ldata}else{data_out <- bind_rows(data_out, ldata)}

  }

  # Return data
  data_use <- data_out %>%
    filter(use==T)

  # Return
  return(data_use)

}
