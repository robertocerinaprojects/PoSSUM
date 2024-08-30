# X API 
Sys.setenv(BEARER_TOKEN = readLines(con = 'tokens/X'))

# set X API bearer token
bearer_token <- Sys.getenv("BEARER_TOKEN")

# Function to create the URL for the Twitter API request (tweets API)
create_url <- function(query, next_token = NULL, max_results, start_time, end_time, lang = lang, exclude_retweets = exclude_retweets) {
  # Fields to be retrieved for each tweet
  tweet_fields <- paste0(
    "attachments,author_id,conversation_id,created_at,entities,geo,id,",
    "in_reply_to_user_id,lang,public_metrics,referenced_tweets,source,text,",
    "possibly_sensitive"
  )
  
  # Fields to be retrieved for each user (based on valid fields from the error message)
  user_fields <- paste0(
    "created_at,description,entities,id,location,most_recent_tweet_id,name,",
    "pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,",
    "verified,verified_type,withheld"
  )
  
  # Base URL for the recent search endpoint
  base_url <- "https://api.twitter.com/2/tweets/search/recent?"
  
  # Exclude retweets from the results if specified
  if (exclude_retweets) {
    query <- paste0(query, " -is:retweet")
  }
  
  # Add a language filter to the query if specified
  if (!is.null(lang)) {
    query <- paste0(query, " lang:", lang)
  }
  
  # Construct the full URL with all parameters
  url <- paste0(base_url,
                "query=", URLencode(query),
                "&max_results=", max_results,
                "&start_time=", start_time,
                "&end_time=", end_time,
                "&tweet.fields=", tweet_fields,
                "&expansions=author_id",
                "&user.fields=", user_fields)
  
  # Add the next_token parameter if provided (used for pagination)
  if (!is.null(next_token)) {
    url <- paste0(url, "&next_token=", next_token)
  }
  
  return(url)
}

# Function to set up the OAuth headers for the API request
bearer_oauth <- function() {
  # Construct the OAuth header using the bearer token
  oauth_header <- paste0("Bearer ", bearer_token)
  return(add_headers(Authorization = oauth_header, `User-Agent` = "v2TweetLookupR"))
}

# Function to connect to the Twitter API endpoint and retrieve data
connect_to_endpoint <- function(url) {
  # Make the GET request to the specified URL with the OAuth headers
  response <- GET(url, config = bearer_oauth())
  
  # Check if the response status is not 200 (OK) and raise an error if so
  if (status_code(response) != 200) {
    stop(paste0("Request returned an error: ", status_code(response), " ", content(response, "text", encoding = "UTF-8")))
  }
  
  # Parse and return the JSON content of the response
  return(content(response, "parsed"))
}

# Main function to search for tweets within a specified time range
search <- function(query, lang, exclude_retweets, start_time, end_time, max_results, store_path, tibble) {
  # List to store all retrieved data
  all_data <- list(tweets = list(), users = list())
  
  # Variable to store the next_token for pagination
  next_token <- NULL
  counter = max_results  # Initialize counter to track fetched results
  
  # Loop to handle pagination and retrieve all results
  repeat {
    
    max_results_per_page = min(100, counter) 
    
    print(paste('collecting:',max_results_per_page,'tweets...'))
    
    # Calculate how many results are still needed
    counter = counter - max_results_per_page 
    
    print(paste('still to collect:',counter,'tweets...'))
    
    
    # Create the URL for the current page of results
    url <- create_url(query, next_token = next_token, start_time = start_time, end_time = end_time, max_results = max_results_per_page, lang = lang, exclude_retweets = exclude_retweets)
    
    print(paste0('call url:\n',url))
    
    
    # Connect to the endpoint and retrieve the data
    json_response <- connect_to_endpoint(url)
    
    # Append the retrieved data to the all_data list
    all_data$tweets <- append(all_data$tweets, json_response$data)
    
    # Check if 'includes' exists in the response and append user data
    if (!is.null(json_response$includes) && !is.null(json_response$includes$users)) {
      all_data$users <- append(all_data$users, json_response$includes$users)
    }
    
    # If I've colleted all I set out to, stop
    if(counter == 0){break}
    
    # Check if there's a next_token in the response (indicating more pages of results)
    if (!is.null(json_response$meta$next_token)) {
      next_token <- json_response$meta$next_token
      Sys.sleep(1) # Introduce a delay to avoid hitting rate limits
      
    } else {
      break # Exit the loop if there are no more pages of results
    }
  }
  
  # Convert the collected data to a JSON format and return
  pool <- fromJSON(toJSON(all_data, pretty = TRUE))
  
  # if save option available, save in field
  if(!is.null(store_path)) { save(pool, file = store_path, compress = TRUE) }
  
  return(pool)
}

# Function to perform the search and handle 429 error
robust_search <- function(query, max_results, l, store_path) {
  attempt <- 1
  max_attempts <- 100
  
  repeat {
    tryCatch({
      
      # refresh time
      search_time <- get_valid_start.end_UTC_time(current_utc_time = get_utc_time())
      
      search.json <- search(
        query = query,
        lang = NULL,
        exclude_retweets = TRUE,
        start_time = search_time$start_time,
        end_time = search_time$end_time,
        max_results = max_results,
        tibble = FALSE,
        store_path = store_path
      )
      return(search.json)
    }, error = function(e) {
      if (grepl("429", e$message)) {
        if (attempt <= max_attempts) {
          Sys.sleep(90)
          attempt <<- attempt + 1
        } else {
          stop("Maximum retry attempts reached. Unable to complete the search.")
        }
      } else {
        stop(e)
      }
    })
  }
}

# Function to create the URL for the Twitter API request (users API)
create_user_timeline_url <- function(user_id, max_results = 100, exclude_retweets = TRUE) {
  # Fields to be retrieved for each tweet
  tweet_fields <- "attachments,author_id,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,public_metrics,referenced_tweets,source,text"
  
  # Base URL for the user timeline endpoint
  base_url <- paste0("https://api.twitter.com/2/users/", user_id, "/tweets?")
  
  # Construct the full URL with all parameters
  url <- paste0(base_url,
                "max_results=", max_results,
                "&tweet.fields=", tweet_fields)
  
  # Exclude retweets if specified
  if (exclude_retweets) {
    url <- paste0(url, "&exclude=retweets")
  }
  
  return(url)
}

# Main function to gather tweets from a specific user
get_user_tweets <- function(user_id, max_results = 100, store_path = NULL) {
  
  # Create the URL for the user's timeline
  url <- create_user_timeline_url(user_id, max_results)
  
  # Connect to the endpoint and retrieve the data
  json_response <- connect_to_endpoint(url)
  
  # Convert the collected data to a JSON format and return
  tweets <- fromJSON(toJSON(json_response, pretty = TRUE))
  
  # if save option available, save in field
  if(!is.null(store_path)) { save(tweets, file = store_path, compress = TRUE) }
  
  return(tweets)
}

# robust version, handling fail due to rate limit via retry 
robust_get_user_tweets <- function(
    user_id, 
    max_results = 100, 
    store_path = NULL,
    max_retries = 100,
    sleep = 120
){
  
  retry_count <- 0
  success <- FALSE
  this.user.tweets <- NULL
  
  while(!success && retry_count < max_retries){
    tryCatch({
      # Attempt to get the user tweets
      this.user.tweets <- get_user_tweets(
        user_id, 
        max_results = max_results,
        store_path = store_path
      )
      # Check if the user has no tweets
      if(is.null(this.user.tweets$data) || length(this.user.tweets$data$text) == 0){
        message('**THIS USER HAS NO TWEETS -- CHECK THAT THIS IS ACTUALLY THE CASE**')
        this.user.tweets$data$user_id = user_id
        this.user.tweets$data$text = 'This user has not produced any original tweets.'
        this.user.tweets$data$created_at = add_noise_to_time(Sys.time())
        return(
          this.user.tweets
        )
      }
      # Save the user tweets if store_path is provided
      if(!is.null(store_path)){
        save(this.user.tweets, file = store_path)
      }
      success <- TRUE
    }, error = function(e) {
      # Print the error details if an error occurs
      cat("Error occurred: ", conditionMessage(e), "\n")
      this.user.tweets <- NULL
    })
    
    if(!success){
      retry_count <- retry_count + 1
      message("Failed to fetch tweets after ", retry_count, " retries...")
      if(retry_count < max_retries){
        message("...trying again in ", sleep, " seconds.")
        Sys.sleep(sleep)
      } else {
        stop("Failed to fetch tweets after ", max_retries, " retries. Stopping execution.")
      }
    }
  }
  
  return(this.user.tweets)
}



# Get trends function -- the url has to be sourced from the country-specific trends24 website
get_trends <- function(url) {

  # Read the content of the webpage
  webpage <- read_html(url)
  
  # Try extracting trends using different CSS selectors
  trends <- webpage %>%
    html_nodes(".list-container:nth-child(1) .trend-link") %>%
    html_text(trim = FALSE)
  
  # could be duplicates - make unique
  trends <- unique(trends)
  
  # If no trends are found, return a warning
  if(length(trends) == 0) {
    warning("No trends found. The website structure might have changed.")
    return(NULL)
  }
  
  
  # # # Depreciated 
  
  # Extract the dates from the trends
  # dates <- trends[grepl("^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}:\\d{2}$", trends)]
  
  # Convert the dates to Date class
  # dates <- as.POSIXct(dates, format="%d-%m-%Y %H:%M:%S")
  
  # Identify the most recent date
  # latest_date <- max(dates)
  
  # Find the index of the latest date
  # index_latest_date <- which(trends == format(latest_date, format="%d-%m-%Y %H:%M:%S"))
  
  # Find the index of the next date after the latest date
  # index_next_date <- which(as.POSIXct(trends, format="%d-%m-%Y %H:%M:%S") < latest_date)[1]
  
  # Extract the trends associated with the latest date
  #if (!is.na(index_next_date)) {
  #  latest_trends <- trends[(index_latest_date + 1):(index_next_date - 1)]
  #} else {
  #  latest_trends <- trends[(index_latest_date + 1):length(trends)]
  #}
  
  # Return 
  return(trends)
}


