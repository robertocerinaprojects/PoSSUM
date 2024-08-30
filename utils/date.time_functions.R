# load existing date-time utils
library(lubridate)

# function to fetch UTC time (useful to interact with the X api)
get_utc_time <- function() {
  response <- GET("http://worldtimeapi.org/api/timezone/Etc/UTC")
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
    datetime <- as.POSIXct(data$utc_datetime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
    formatted_datetime <- format(datetime, "%Y-%m-%dT%H:%M:%SZ")
    return(formatted_datetime)
  } else {
    stop("Failed to fetch UTC time.")
  }
}

# function to extract a date and time from a set of file names
extract_latest_date <- function(filenames, time = FALSE) {
  # Extract dates using regex
  date_time_strings <- gsub(".*(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}).*", "\\1", filenames)
  
  # Convert to POSIXct
  date_times <- as.POSIXct(date_time_strings, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Get the latest date-time
  latest_date_time <- max(date_times, na.rm = TRUE)
  
  # Return the latest date-time based on the time argument
  if (time) {
    return(latest_date_time)
  } else {
    return(as.Date(latest_date_time))
  }
}

# get start and end time 
get_valid_start.end_UTC_time <- 
  function(
    current_utc_time, 
    start_seconds_prior = 60*60*24*7 - 60, # default is just under a week ago
    end_seconds_prior = 10 # default is 10 seconds before now, due to X api doc.
    ){ 
  
  # format current date time
  datetime <- as.POSIXct(current_utc_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

  # get start time
  start_time <- format(datetime - start_seconds_prior, "%Y-%m-%dT%H:%M:%SZ")
  
  # get end time
  end_time <- format(datetime - end_seconds_prior, "%Y-%m-%dT%H:%M:%SZ")
  
  return(list(start_time = start_time,end_time = end_time))
}

# add noise to time 
add_noise_to_time <- function(date_str,h = 12) {
  # Load necessary library
  library(lubridate)
  
  # Parse the input date string
  original_time <- ymd_hms(date_str)
  
  # Generate a random number of seconds within ±12 hours
  noise_seconds <- sample((-h*60*60):(h*60*60), 1)
  
  # Add noise to the original time
  new_time <- original_time + dseconds(noise_seconds)
  
  # Return the new time in the same format
  return(format(new_time, "%Y-%m-%dT%H:%M:%S.000Z"))
}

