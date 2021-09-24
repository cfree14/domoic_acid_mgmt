
make_mgmt_decisions <- function(toxin_grid_df, survey_results, stations, mgmt_zones,
                                test_interval, close_thresh){

  # Identify day last contam
  identify_last_day_contam <- function(statuses, days){

    # Loop through days
    df <- purrr::map_df(days, function(x){
      day_do <- x
      days_contaminated <- days[statuses=="contaminated"]
      day_contaminated_before_day_do <- days_contaminated[days_contaminated<=day_do]
      if(length(day_contaminated_before_day_do)>0){
        last_day <- max(day_contaminated_before_day_do)
      }else{
        last_day <- NA
      }
      out <- tibble(day=day_do,
                    last_day=last_day)
    })
    last_days <- df %>% pull(last_day)

    return(last_days)
  }

  # Function to identify closure runs
  identify_runs <- function(vec, test_interval){
    run_list <- split(vec, cumsum(c(1, diff(vec) != test_interval*7)))
    run_df <- purrr::map_df(1:length(run_list), function(x){
      run_id <- names(run_list)[x]
      run_vals <- run_list[[x]]
      run_id_rep <- rep(run_id, length(run_vals)) %>%  as.numeric()
      df <- tibble(run_id=run_id_rep, day=run_vals)
    })
    run_id <- run_df$run_id
    return(run_id)
  }

  # If closures...
  if(sum(survey_results$status_obs=="contaminated")>0){

    # Identify closure dates
    closure_dates <- survey_results %>%
      # Mark contaminated days as closures
      mutate(mgmt_status=ifelse(status_obs=="contaminated", "closed", NA)) %>%
      # Mark days >2 sampling intervals after contamination as closed
      group_by(lat) %>%
      mutate(contam_last_day=identify_last_day_contam(status_obs, day),
             days_since_last_contam=day-contam_last_day) %>%
      ungroup() %>%
      mutate(mgmt_status=ifelse(days_since_last_contam <= 14 & !is.na(days_since_last_contam), "closed", mgmt_status)) %>%
      # Mark open point
      mutate(mgmt_status=ifelse(is.na(mgmt_status), "open", mgmt_status)) %>%
      # Reduce to closures
      filter(mgmt_status=="closed") %>%
      # Mark distinct closures (runs of consecutive test)
      group_by(station_id, lat) %>%
      mutate(closure=identify_runs(vec=day, test_interval)) %>%
      ungroup() %>%
      # Summarize by closure
      group_by(station_id, lat, closure) %>%
      summarize(day1=min(day),
                day2=max(day))

    # Expand closure dates
    closure_dates_expanded <- purrr::map_df(1:nrow(closure_dates), function(x){

      # Extract parameters
      station_id <- closure_dates$station_id[x]
      lat <- closure_dates$lat[x]
      day1 <- closure_dates$day1[x]
      day2 <- closure_dates$day2[x]
      days <- day1:day2

      # Build dataframe
      df <- tibble(station_id=station_id,
                   lat=lat,
                   day=days,
                   mgmt_status="Closed")

    })

    # Build management grid
    mgmt_zones_padded <- c(mgmt_zones[1]-1, mgmt_zones[2:length(mgmt_zones)])
    mgmt_grid_df <- toxin_grid_df %>%
      # Add management zone
      mutate(mgmt_zone=cut(lat, breaks=mgmt_zones_padded, labels=1:length(stations)) %>% as.numeric()) %>%
      # Add true status
      mutate(status_true=ifelse(prop<=close_thresh, "Open", "Closed")) %>%
      # Arrange
      select(mgmt_zone, lat, day, prop, status_true) %>%
      # Add management status
      left_join(closure_dates_expanded %>% select(-lat), by=c("mgmt_zone"="station_id", "day")) %>%
      rename(status_used=mgmt_status) %>%
      mutate(status_used=ifelse(is.na(status_used), "Open", status_used),
             status_used=factor(status_used, levels=c("Closed", "Open"))) %>%
      # Classify performance
      mutate(correct_yn=status_true==status_used,
             status_diff=paste(status_true, status_used, sep="-"),
             status_diff=recode_factor(status_diff,
                                       "Open-Open"="Open correctly",
                                       "Closed-Closed"="Closed correctly",
                                       "Open-Closed"="Closed unnecessarily",
                                       "Closed-Open"="Open riskily"))

  # If no closures...
  }else{

    # Build management grid
    mgmt_zones_padded <- c(mgmt_zones[1]-1, mgmt_zones[2:length(mgmt_zones)])
    mgmt_grid_df <- toxin_grid_df %>%
      # Add management zone
      mutate(mgmt_zone=cut(lat, breaks=mgmt_zones_padded, labels=1:length(stations)) %>% as.numeric()) %>%
      # Add true status
      mutate(status_true=ifelse(prop<=close_thresh, "Open", "Closed")) %>%
      # Arrange
      select(mgmt_zone, lat, day, prop, status_true) %>%
      # Add management status
      mutate(status_used="Open",
             status_used=factor(status_used, levels=c("Closed", "Open"))) %>%
      # Classify performance
      mutate(correct_yn=status_true==status_used,
             status_diff=paste(status_true, status_used, sep="-"),
             status_diff=recode_factor(status_diff,
                                       "Open-Open"="Open correctly",
                                       "Closed-Closed"="Closed correctly",
                                       "Open-Closed"="Closed unnecessarily",
                                       "Closed-Open"="Open riskily"))

  }

  # Return
  return(mgmt_grid_df)

}
