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
  result <- fromJSON(toJSON(all_data, pretty = TRUE))
  
  # if save option available, save in field
  if(!is.null(store_path)) { save(result, file = store_path, compress = TRUE) }
  
  return(result)
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
  result <- fromJSON(toJSON(json_response, pretty = TRUE))
  
  # if save option available, save in field
  if(!is.null(store_path)) { save(result, file = store_path, compress = TRUE) }
  
  return(result)
}
